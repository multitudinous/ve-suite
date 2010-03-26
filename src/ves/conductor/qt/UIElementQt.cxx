
// --- VES includes --- //
#include <ves/conductor/qt/UIElementQt.h>
#include <ves/xplorer/util/InteractionEvent.h>



// --- C++ includes --- //
#include <cassert>
#include <iostream>
#include <sstream>

// --- Qt includes --- //
#include <QtGui/QBrush>
#include <QtCore/QPoint>
#include <QtGui/QMouseEvent>
#include <QtCore/QCoreApplication>
#include <QtCore/QMutexLocker>
#include <QtGui/QApplication>

// qt_sendSpontaneousEvent is a friend method of QCoreApplication and thus
// gives us access to the private method sendSpontaneousEvent, which is required
// to propagate mouse and keyboard events to the off-screen Qt element as though
// the events came directly from the window manager. Event propagation will
// not work correctly without this.

bool qt_sendSpontaneousEvent( QObject* recv, QEvent* e )
{
    return QCoreApplication::sendSpontaneousEvent( recv, e );
}

using namespace ves::conductor;

UIElementQt::UIElementQt( QWidget *parent ) : QGraphicsView( parent )
{
    _debug( "ctor" );
    mWidget = NULL;
    mImage = NULL;
    mImageFlipped = NULL;
    mGraphicsScene = NULL;
    mGraphicsProxyWidget = NULL;
    mGraphicsView = NULL;
    mTimer = NULL;
    mImageWidth = 0;
    mImageHeight = 0;
    mWidth = 0;
    mHeight = 0;
    mTextureLeft = 0.0f;
    mTextureRight = 0.0f;
    mTextureBottom = 0.0f;
    mTextureTop = 0.0f;
    mInitialized = false;
    mImageDirty = true;
    mDirty = true;
    this->setCacheMode( QGraphicsView::CacheNone );
    setRenderHints( QPainter::NonCosmeticDefaultPen );
    QBrush brush( QColor( 0, 0, 0, 0 ) );
    brush.setStyle( Qt::SolidPattern );
    this->setBackgroundBrush( brush );
    mImageMutex = new QMutex( );
    Initialize();
}

UIElementQt::~UIElementQt( )
{
    _debug( "dtor" );
    FreeOldWidgets( );
    delete mTimer;
}

void UIElementQt::Initialize( )
{
    _debug( "Initialize" );
    if( !mInitialized )
    {
        // Note that all signals that might be fired based on execution in another
        // thread and that call slots in this class must be
        // of mode Qt::QueuedConnection so that the slot will only be processed
        // when the thread that created this object has execution.
        //QObject::connect( this, SIGNAL( RequestRender( ) ), this, SLOT( _render( ) ), Qt::QueuedConnection );
        QObject::connect( this, SIGNAL( PutSendEvent( xplorer::util::InteractionEvent* ) ),
                          this, SLOT( _sendEvent( xplorer::util::InteractionEvent* ) ), Qt::QueuedConnection );
        QObject::connect( this, SIGNAL( RequestEmbed( bool ) ), this, SLOT( _embed( bool ) ), Qt::QueuedConnection );

        // Start up the timer that causes repaints at a set interval -- assuming
        // thread is given execution sometime during this interval.
        mTimer = new QTimer( this );
        QObject::connect( mTimer, SIGNAL( timeout( ) ), this, SLOT( _render( ) ) );
        mTimer->start( 30 );

        mInitialized = true;
    }
}

int UIElementQt::GetImageWidth( )
{
    return mImageWidth;
}

int UIElementQt::GetImageHeight( )
{
    return mImageHeight;
}

int UIElementQt::GetElementWidth( )
{
    return mWidth;
}

int UIElementQt::GetElementHeight( )
{
    return mHeight;
}

const osg::Vec4f UIElementQt::GetTextureCoordinates( )
{
    osg::Vec4f m_coordinates;

    // Vec4f.set takes args in order x, y, z, w, thus the odd placement of mLeft
    m_coordinates.set( mTextureRight, mTextureBottom,
                       mTextureTop, mTextureLeft );
    return m_coordinates;
}

void UIElementQt::SendInteractionEvent( xplorer::util::InteractionEvent
                                        &event )
{
    xplorer::util::InteractionEvent* m_event =
            new xplorer::util::InteractionEvent( event );

    Q_EMIT PutSendEvent( m_event );
}

unsigned char* UIElementQt::RenderElementToImage( )
{
    //_debug( "RenderElementToImage" );
    // If there's no widget to render, return NULL
    if( mWidget == NULL )
    {
        return NULL;
    }

    // Qt's event and paint system behaves poorly if initiated directly from
    // another thread, so we do a little trickery here. This method doesn't
    // actually ask the element to update; instead, we keep an internal timer
    // that causes a repaint of this element every 30ms. Any time the present
    // method is called, we simply return the most recently painted image of
    // this window. Attempts to cause this method to ask the element to update,
    // which could be done via:
    //Q_EMIT RequestRender( );
    // do not seem to work well. It appears that the Qt thread is not given
    // execution time frequently enough, and the end effect is that the painting
    // lags far behind juggler's event queue.

    // Flip mImage vertically and store in mImageFlipped. OpenGL treats textures
    // as scanning from bottom up, but Qt renders images top down, so we have to
    // do this little flip trick.
    if( mImageDirty )
    {
        _debug( "RenderElementToImage...Flipping image" );
        { // Enter critical section
            QMutexLocker locker( mImageMutex );
            ( *mImageFlipped ) = mImage->mirrored( false, true );
        } // Leave critical section
        mImageDirty = false;
        mDirty = true;
    }
    else
    {
        mDirty = false;
    }
    return mImageFlipped->bits( );
}

bool UIElementQt::IsDirty()
{
    return mDirty;
}

void UIElementQt::SetWidget( QWidget* widget )
{
    _debug( "SetWidget" );
    // Exit immediately if widget is null
    if( widget == NULL )
    {
        mWidget = NULL;
        return;
    }

    // Clean up any widgets this instance previously owned so we don't leak
    // memory
    FreeOldWidgets( );

    mWidget = widget;

    UpdateSize( );

    // Create a scene and a view to add this widget into
    mGraphicsScene = new QGraphicsScene( 0, 0, mWidth, mHeight );
    this->setScene( mGraphicsScene );

    // Force the scene to be aligned into the top-left corner of the view.
    // The default is centered, which can result in odd alignment
    this->setAlignment( Qt::AlignLeft | Qt::AlignTop );

    // Add the widget to the scene.
    // NB: We don't have to explicitly create mGraphicsProxyWidget since
    // it is created and returned by call to QGraphicsScene::addWidget
    mGraphicsProxyWidget = mGraphicsScene->addWidget( mWidget );
    mGraphicsProxyWidget->show( );

    // Ensure that mWidget is in the top left-hand corner of mGraphicsScene.
    mWidget->move( 0, 0 );

    // Should we be responsible for this here? What if widget designer
    // doesn't want it to have mouseTracking?
    mWidget->setMouseTracking( true );

    // mWidget cannot get mouse move events unless we turn them on for
    // GraphicsView.
    this->setMouseTracking( true );

    // Turn off view scrollbars since they muck everything up
    this->setHorizontalScrollBarPolicy( Qt::ScrollBarAlwaysOff );
    this->setVerticalScrollBarPolicy( Qt::ScrollBarAlwaysOff );

    // Make the view and the scene coincident.
    this->setSceneRect( mGraphicsScene->sceneRect( ) );

    // Forcibly resize view to contain mWidget
    this->resize( mWidth, mHeight );

    // Render this widget immediately so that there will be an image to return
    // to the first call to RenderElementToImage()
    _render( );

    // Force the scene to activate. Failure to do this results in lack of
    // tab-focus feedback and odd cursor behavior on Qt 4.6.0. Unfortunately,
    // doing the activate on the scene causes comboboxes in the property browser
    // to stop showing choices on pulldown. Ugh.
    //QEvent ev( QEvent::WindowActivate );
    //QApplication::sendEvent( mGraphicsScene, &ev );
}

void UIElementQt::UpdateSize( )
{
    _debug( "UpdateSize" );
    int temp_Width = mWidth;
    int temp_Height = mHeight;

    mWidth = mWidget->width( );
    mHeight = mWidget->height( );

    //Line for debugging
    //std::cout << mWidth << ", " << mHeight << std::endl;

    // Delete the image and flipped image object if the required texture size
    // has changed.
    if( ( temp_Width != mWidth ) || ( temp_Height != mHeight ) )
    {
        { // Enter critical section
            QMutexLocker locker( mImageMutex );
            if( mImage )
            {
                delete mImage;
            }
        } // Leave critical section

        if( mImageFlipped )
        {
            delete mImageFlipped;
        }
    }

    // Determine the texture size needed to ensure power-of-two width and height
    _calculatePower2ImageDimensions( );

    // Calculate the texture coordinates of the rendered widget in the texture
    _calculateTextureCoordinates( );

    // If image doesn't exist, create one of the proper size.
    { // Enter critical section
        QMutexLocker locker( mImageMutex );
        if( mImage == NULL )
        {
            mImage = new QImage( mImageWidth, mImageHeight, QImage::Format_ARGB32_Premultiplied );
        }
    } // Leave critical section
    if( mImageFlipped == NULL )
    {
        mImageFlipped = new QImage( mImageWidth, mImageHeight, QImage::Format_ARGB32_Premultiplied );
    }
}

void UIElementQt::FreeOldWidgets( )
{
    _debug( "FreeOldWidgets" );
    // ?? Should we manage deletion of the owned widget? What if
    // something else also owns it? Don't delete it. Embedding should be
    // different from ownership.

    //delete mWidget;
    delete mGraphicsScene;
    delete mGraphicsView;
    delete mGraphicsProxyWidget;

    { // Enter critical section
        QMutexLocker locker( mImageMutex );
        delete mImage;
    } // Leave critical section

    delete mImageFlipped;
}

void UIElementQt::_render( )
{
    _debug( "_render" );
    // If there's no widget to render, return NULL
    if( mWidget == NULL )
    {
        return;
    }


    { // Enter critical section
        QMutexLocker locker( mImageMutex );

        if( mImage )
        {

            // Clear mImage to empty transparency
            mImage->fill( 0 );

            // Render element into mImage
            QPainter m_imagePainter( mImage );
            QRect m_Rectangle( 0, 0, mWidth, mHeight );
            this->render( &m_imagePainter, m_Rectangle, m_Rectangle );
            m_imagePainter.end( );
        }
    } // Leave critical section

    mImageDirty = true;
}

void UIElementQt::_calculatePower2ImageDimensions( )
{
    mImageWidth = int( pow( 2, ceil( log( mWidth ) / log( 2 ) ) ) );
    mImageHeight = int( pow( 2, ceil( log( mHeight ) / log( 2 ) ) ) );

    // If either dimension is zero, force the other dimension to zero too
    if( ( mImageWidth == 0 ) || ( mImageHeight == 0 ) )
    {
        mImageWidth = mImageHeight = 0;
    }
}

void UIElementQt::_calculateTextureCoordinates( )
{
    // Save ourselves from divide-by-zero
    if( mImageWidth == 0 )
    {
        mTextureLeft = mTextureRight = mTextureBottom = mTextureTop = 0.0f;
    }
    else
    {
        mTextureLeft = 0.0f;
        mTextureRight = static_cast < float > ( mWidth ) /
                static_cast < float > ( mImageWidth );
        mTextureBottom = 1.0f - ( static_cast < float > ( mHeight ) /
                static_cast < float > ( mImageHeight ) );
        mTextureTop = 1.0f;
    }
}

void UIElementQt::_debug( const std::string text )
{
//#define UIELEMENTQT_DEBUG
#ifdef UIELEMENTQT_DEBUG
    std::cout << "UIElementQt::" << text << std::endl << std::flush;
#endif
}

void UIElementQt::paintEvent( QPaintEvent* event )
{
    _debug( "paintEvent" );
    _render( );
    QGraphicsView::paintEvent( event );
}

void UIElementQt::_sendEvent( xplorer::util::InteractionEvent* event )
{
    int x = static_cast < int > ( event->X );
    int y = static_cast < int > ( event->Y );

    QPoint globalPos = this->viewport( )->mapToGlobal( QPoint( x, y ) );
    QWidget *vw = this->childAt( x, y );

    // May not want to do this line if keyboard event...
    if( vw == NULL ) return;

    // keyPress, keyRelease, scroll
    // This is incomplete.
    // 1. Does not deal with key events or scroll events
    // 2. Always assumes left mouse button and never actually looks at which button.
    // 3. Does not look at modifier keys.
    switch( event->EventType )
    {
    case xplorer::util::InteractionEvent::buttonPress:
    {
        QMouseEvent e( QEvent::MouseButtonPress, QPoint( x, y ), globalPos, Qt::LeftButton,
                       Qt::LeftButton, Qt::NoModifier );
        qt_sendSpontaneousEvent( this->viewport( ), &e );
    }
        break;
    case xplorer::util::InteractionEvent::buttonRelease:
    {
        QMouseEvent e( QEvent::MouseButtonRelease, QPoint( x, y ), globalPos, Qt::LeftButton,
                       Qt::LeftButton, Qt::NoModifier );
        qt_sendSpontaneousEvent( this->viewport( ), &e );
    }
        break;
    case xplorer::util::InteractionEvent::pointerMotion:
    {
        if( event->Buttons == xplorer::util::InteractionEvent::none )
        {
            QMouseEvent e( QEvent::MouseMove, QPoint( x, y ), globalPos, Qt::NoButton,
                           Qt::NoButton, Qt::NoModifier );
            qt_sendSpontaneousEvent( this->viewport( ), &e );
        }
        else
        {
            QMouseEvent e( QEvent::MouseMove, QPoint( x, y ), globalPos, Qt::NoButton,
                           Qt::LeftButton, Qt::NoModifier );
            qt_sendSpontaneousEvent( this->viewport( ), &e );
        }
    }
        break;
    }

    // We're done with the passed event; free its memory.
    delete event;
}

void UIElementQt::Unembed( )
{
    Q_EMIT RequestEmbed( false );
}

void UIElementQt::Embed( )
{
    Q_EMIT RequestEmbed( true );
}

void UIElementQt::_embed( bool embed )
{
    if( embed )
    {
        this->hide( );
        if( !mTimer->isActive( ) )
        {
            mTimer->start( 30 );
        }
    }
    else
    {
        if( mTimer->isActive( ) )
        {
            mTimer->stop( );
        }
        this->show( );
    }
}
