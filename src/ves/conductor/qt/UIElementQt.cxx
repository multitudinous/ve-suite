/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2010 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/
//#define VES_QT_RENDER_DEBUG

#include <gadget/Type/KeyboardMouse/Keys.h>

// --- VES includes --- //
#include <ves/conductor/qt/UIElementQt.h>
#include <ves/xplorer/eventmanager/InteractionEvent.h>
#include <ves/conductor/qt/ui_titlebar.h>
//#include <ves/conductor/qt/MoveFrame.h>

// --- Juggler includes --- //
#include <gadget/Type/KeyboardMouse/Keys.h>

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
#include <ves/conductor/qt/UIManager.h>

#ifdef VES_QT_RENDER_DEBUG
#define _debug(text) std::cout << "UIElementQt::" << text << std::endl << std::flush
#else
#define _debug(text)
#endif

// qt_sendSpontaneousEvent is a friend method of QCoreApplication and thus
// gives us access to the private method sendSpontaneousEvent, which is required
// to propagate mouse and keyboard events to the off-screen Qt element as though
// the events came directly from the window manager. Event propagation will
// not work correctly without this.

bool qt_sendSpontaneousEvent( QObject* recv, QEvent* e )
{
    return QCoreApplication::sendSpontaneousEvent( recv, e );
}

Q_DECLARE_METATYPE(gadget::Keys)
//Q_DECLARE_METATYPE(wchar_t)

namespace ves
{
namespace conductor
{
////////////////////////////////////////////////////////////////////////////////
UIElementQt::UIElementQt( QWidget *parent ) :
QGraphicsView( parent ),
UIElement(),
mWidget( 0 ),
mImage( 0 ),
mImageFlipped( 0 ),
mGraphicsScene( 0 ),
mGraphicsProxyWidget( 0 ),
mGraphicsView( 0 ),
mImageWidth( 0 ),
mImageHeight( 0 ),
mWidth( 0 ),
mHeight( 0 ),
mTextureLeft( 0.0f ),
mTextureRight( 1.0f ),
mTextureBottom( 0.0f ),
mTextureTop( 1.0f ),
mInitialized( false ),
mImageDirty( true ),
mTimer( 0 ),
mImageMutex( 0 ),
mDirty( true ),
mTitlebar( 0 ),
mQTitlebar( 0 )
{
    _debug( "ctor" );

    SetMinimized( false );

    this->setCacheMode( QGraphicsView::CacheNone );
    setRenderHints( QPainter::NonCosmeticDefaultPen );
    QBrush brush( QColor( 0, 0, 0, 0 ) );
    brush.setStyle( Qt::SolidPattern );
    this->setBackgroundBrush( brush );
    mImageMutex = new QMutex();
    _setupKeyMap();
    
    mQTitlebar = new Ui::titlebar();
    mTitlebar = new QWidget( 0 );
    mQTitlebar->setupUi( mTitlebar );

    Initialize();
    PostConstructor();
}
////////////////////////////////////////////////////////////////////////////////
UIElementQt::~UIElementQt()
{
    _debug( "dtor" );
    FreeOldWidgets();
    delete mTimer;
    delete mImageMutex;
    delete mQTitlebar;
}
////////////////////////////////////////////////////////////////////////////////
void UIElementQt::Initialize()
{
    _debug( "Initialize" );
    if( !mInitialized )
    {
        // Note that all signals that might be fired based on execution in another
        // thread and that call slots in this class must be
        // of mode Qt::QueuedConnection so that the slot will only be processed
        // when the thread that created this object has execution.
        //QObject::connect( this, SIGNAL( RequestRender() ), this, SLOT( _render() ), Qt::QueuedConnection );
        //QObject::connect( this, SIGNAL( PutSendEvent( ves::xplorer::eventmanager::InteractionEvent* ) ),
        //                  this, SLOT( _sendEvent( ves::xplorer::eventmanager::InteractionEvent* ) ), Qt::QueuedConnection );

        QObject::connect( this, SIGNAL( PutResizeCanvas( int, int) ),
                          this, SLOT( _resizeCanvas( int, int ) ), Qt::QueuedConnection );

        qRegisterMetaType<gadget::Keys>();
        //qRegisterMetaType<wchar_t>();

        QObject::connect( this, SIGNAL( PutButtonPressEvent( gadget::Keys, int, int, int ) ),
                          this, SLOT( _buttonPressEvent( gadget::Keys, int, int, int ) ), Qt::QueuedConnection );

        QObject::connect( this, SIGNAL( PutButtonReleaseEvent( gadget::Keys, int, int, int ) ),
                          this, SLOT( _buttonReleaseEvent( gadget::Keys, int, int, int ) ), Qt::QueuedConnection );

        QObject::connect( this, SIGNAL( PutDoubleClickEvent( gadget::Keys, int, int, int ) ),
                          this, SLOT( _doubleClickEvent( gadget::Keys, int, int, int ) ), Qt::QueuedConnection );

        QObject::connect( this, SIGNAL( PutMouseMoveEvent( int, int, int, int ) ),
                          this, SLOT( _mouseMoveEvent( int, int, int, int ) ), Qt::QueuedConnection );

        QObject::connect( this, SIGNAL( PutKeyPressEvent( gadget::Keys, int, char ) ),
                          this, SLOT( _keyPressEvent( gadget::Keys, int, char ) ), Qt::QueuedConnection );
        //QObject::connect( this, SIGNAL( PutKeyPressEvent( gadget::Keys, int, QString ) ),
        //                 this, SLOT( _keyPressEvent( gadget::Keys, int, QString ) ), Qt::QueuedConnection );

        QObject::connect( this, SIGNAL( PutKeyReleaseEvent( gadget::Keys, int, char ) ),
                          this, SLOT( _keyReleaseEvent( gadget::Keys, int, char ) ), Qt::QueuedConnection );
        //QObject::connect( this, SIGNAL( PutKeyReleaseEvent( gadget::Keys, int, QString ) ),
        //                 this, SLOT( _keyReleaseEvent( gadget::Keys, int, QString ) ), Qt::QueuedConnection );

        // Start up the timer that causes repaints at a set interval -- assuming
        // thread is given execution sometime during this interval.
        mTimer = new QTimer( this );
        QObject::connect( mTimer, SIGNAL( timeout() ), this, SLOT( _render() ) );
        mTimer->start( 30 );

        QObject::connect( mQTitlebar->OpacitySlider, SIGNAL( valueChanged( int ) ), this, SLOT( _onOpacitySliderValueChanged( int ) ) );
        QObject::connect( mQTitlebar->HideButton, SIGNAL( clicked() ), this, SLOT( _onHideButtonClicked() ) );
        QObject::connect( mQTitlebar->MinimizeButton, SIGNAL( clicked() ), this, SLOT( _onMinimizeButtonClicked() ) );
        QObject::connect( mQTitlebar->TitleFrame, SIGNAL( pressed() ), this, SLOT( _onTitlebarPressed() ) );

        mInitialized = true;
    }
    _debug( "Initialize Ended" );
}
////////////////////////////////////////////////////////////////////////////////
int UIElementQt::GetImageWidth()
{
    return mImageWidth;
}
////////////////////////////////////////////////////////////////////////////////
int UIElementQt::GetImageHeight()
{
    return mImageHeight;
}
////////////////////////////////////////////////////////////////////////////////
int UIElementQt::GetElementWidth()
{
    return mWidth;
}
////////////////////////////////////////////////////////////////////////////////
int UIElementQt::GetElementHeight()
{
    return mHeight;
}
////////////////////////////////////////////////////////////////////////////////
const osg::Vec4f UIElementQt::GetTextureCoordinates()
{
    osg::Vec4f m_coordinates;

    // Vec4f.set takes args in order x, y, z, w, thus the odd placement of mLeft
    m_coordinates.set( mTextureRight, mTextureBottom,
                       mTextureTop, mTextureLeft );
    return m_coordinates;
}
////////////////////////////////////////////////////////////////////////////////
void UIElementQt::SendInteractionEvent( ves::xplorer::eventmanager::InteractionEvent&
                                        event )
{
//    ves::xplorer::eventmanager::InteractionEvent* m_event =
//            new ves::xplorer::eventmanager::InteractionEvent( event );
//
//    Q_EMIT PutSendEvent( m_event );
}
////////////////////////////////////////////////////////////////////////////////
void UIElementQt::SendButtonPressEvent( gadget::Keys button, int x, int y, int state )
{
    Q_EMIT PutButtonPressEvent( button, x, y, state );
}
////////////////////////////////////////////////////////////////////////////////
void UIElementQt::SendButtonReleaseEvent( gadget::Keys button, int x, int y, int state )
{
    Q_EMIT PutButtonReleaseEvent( button, x, y, state );
}
////////////////////////////////////////////////////////////////////////////////
void UIElementQt::SendDoubleClickEvent( gadget::Keys button, int x, int y, int state )
{
    Q_EMIT PutDoubleClickEvent( button, x, y, state );
}
////////////////////////////////////////////////////////////////////////////////
void UIElementQt::SendMouseMoveEvent( int x, int y, int z, int state )
{
    Q_EMIT PutMouseMoveEvent( x, y, z, state );
}
////////////////////////////////////////////////////////////////////////////////
void UIElementQt::SendKeyPressEvent( gadget::Keys key, int modifierMask, char unicode )
{
    //Right now VR Juggler has the API to support wide body chars but it 
    //is not wired up. When it is we can use this code to transfer wide body
    //chars to Qt.
    //wchar_t uniKey = unicode;
    //QString qUniKey = QString::fromWCharArray( &uniKey, 1 );
    Q_EMIT PutKeyPressEvent( key, modifierMask, unicode );
}
////////////////////////////////////////////////////////////////////////////////
void UIElementQt::SendKeyReleaseEvent( gadget::Keys key, int modifierMask, char unicode )
{
    //Right now VR Juggler has the API to support wide body chars but it 
    //is not wired up. When it is we can use this code to transfer wide body
    //chars to Qt.
    //wchar_t uniKey = unicode;
    //QString qUniKey = QString::fromWCharArray( &uniKey, 1 );
    Q_EMIT PutKeyReleaseEvent( key, modifierMask, unicode );
}
////////////////////////////////////////////////////////////////////////////////
unsigned char* UIElementQt::RenderElementToImage()
{
    //_debug( "RenderElementToImage" );
    // If there's no widget to render, return NULL
    if( mWidget == NULL )
    {
        return NULL;
    }

    // This method doesn't cause the element to repaint;
    // instead, we keep an internal timer that causes a repaint of this element
    // every 30ms. Any time the present method is called, we simply return the
    // most recently painted image of this window.

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
    return mImageFlipped->bits();
}
////////////////////////////////////////////////////////////////////////////////
bool UIElementQt::IsDirty()
{
    return mDirty;
}
////////////////////////////////////////////////////////////////////////////////
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
    FreeOldWidgets();

    mWidget = widget;

    //SetWidget is initializing all of the components for this UIElement
    //therefore we will use the initial image size to determine all sizing
    //for this UIElement.
    mWidth = m_initialImageSize.first;
    mHeight = m_initialImageSize.second;

    // Create a scene and a view to add this widget into
    mGraphicsScene = new QGraphicsScene( 0, 0, mWidth, mHeight );
    this->setScene( mGraphicsScene );

    // Force the scene to be aligned into the top-left corner of the view.
    // The default is centered, which can result in odd alignment
    this->setAlignment( Qt::AlignLeft | Qt::AlignTop );

    // Add our internal titlebar to the top of the widget.
    QGraphicsProxyWidget* titlebar = mGraphicsScene->addWidget( mTitlebar );
    mTitlebar->resize( mWidth, mTitlebar->height() );
    titlebar->show();
    mTitlebar->move( 0, 0 );
    //Now resize the respective widget to account for the title bar
    mWidget->resize( mWidth, mHeight - mTitlebar->height() );
    
    // Add the widget to the scene.
    // NB: We don't have to explicitly create mGraphicsProxyWidget since
    // it is created and returned by call to QGraphicsScene::addWidget
    mGraphicsProxyWidget = mGraphicsScene->addWidget( mWidget );
    mGraphicsProxyWidget->show();

    // Ensure that mWidget is in the top left-hand corner of mGraphicsScene.
    //mWidget->move( 0, 0 );
    mWidget->move( 0, mTitlebar->height() );

    // Should we be responsible for this here? What if widget designer
    // doesn't want it to have mouseTracking?
    mWidget->setMouseTracking( true );

    // mWidget cannot get mouse move events unless we turn them on for
    // GraphicsView.
    this->setMouseTracking( true );

    // Turn off view scrollbars since they muck everything up
    this->setHorizontalScrollBarPolicy( Qt::ScrollBarAlwaysOff );
    this->setVerticalScrollBarPolicy( Qt::ScrollBarAlwaysOff );

    this->setViewportUpdateMode( QGraphicsView::NoViewportUpdate );

    mGraphicsScene->setSceneRect( 0, 0, mWidth, mHeight );

    // Make the view and the scene coincident.
    this->setSceneRect( mGraphicsScene->sceneRect() );

    // Forcibly resize view to contain mWidget
    this->resize( mWidth, mHeight );

    //Now before we render but after all the widgets are configured
    //lets initialize and update all of our textures
    UpdateSize();

    // Render this widget immediately so that there will be an image to return
    // to the first call to RenderElementToImage()
    _render();

#ifdef VES_QT_RENDER_DEBUG
    // Dump the image from the initial render pass into a file
    mImage->save( "QtRenderDebugImage.png" );
#endif

    // Force the scene to activate. Failure to do this results in lack of
    // tab-focus feedback and odd cursor behavior on Qt 4.6.x. Unfortunately,
    // doing the activate on the scene causes comboboxes in the property browser
    // to stop showing choices on pulldown. Ugh.
    //    QEvent ev( QEvent::WindowActivate );
    //    QApplication::sendEvent( mGraphicsScene, &ev );
}
////////////////////////////////////////////////////////////////////////////////
void UIElementQt::ResizeCanvas( int width, int height )
{
    // Emit a queued signal just in case this is called from the "wrong" thread
    PutResizeCanvas( width, height );
}
////////////////////////////////////////////////////////////////////////////////
void UIElementQt::_resizeCanvas( int width, int height )
{
//    mWidget->resize( width, height - mTitlebar->height()  );
//    mTitlebar->resize( width, mTitlebar->height() );
//    UpdateSize();
//    mGraphicsScene->setSceneRect( 0, 0, mWidth, mHeight );
//    this->resize( mWidth, mHeight );
}
////////////////////////////////////////////////////////////////////////////////
void UIElementQt::UpdateSize()
{
    _debug( "UpdateSize" );
    int old_Width = mWidth;
    int old_Height = mHeight;

    mWidth = mWidget->width();
    mHeight = mWidget->height() + mTitlebar->height();

    mElementMatrix.makeScale( mWidth, mHeight, 1);
    mElementMatrixDirty = true;

    // Delete the image and flipped image object if the required texture size
    // has changed.
    if( ( mWidth != old_Width ) || ( mHeight != old_Height ) )
    {
        { // Enter critical section
            QMutexLocker locker( mImageMutex );
            if( mImage )
            {
                delete mImage;
                mImage = 0;
                mImageDirty = true;
            }
            if( mImageFlipped )
            {
                delete mImageFlipped;
                mImageFlipped = 0;
            }
        } // Leave critical section
    }

    mImageWidth = mWidth;
    mImageHeight = mHeight;

    // If image doesn't exist, create one of the proper size.
    { // Enter critical section
        QMutexLocker locker( mImageMutex );
        if( mImage == NULL )
        {
            mImage = new QImage( mImageWidth, mImageHeight, QImage::Format_ARGB32_Premultiplied );
        }
        if( mImageFlipped == NULL )
        {
            mImageFlipped = new QImage( mImageWidth, mImageHeight, QImage::Format_ARGB32_Premultiplied );
        }
    } // Leave critical section
}
////////////////////////////////////////////////////////////////////////////////
void UIElementQt::FreeOldWidgets()
{
    _debug( "FreeOldWidgets" );
    // ?? Should we manage deletion of the owned widget? What if
    // something else also owns it? Don't delete it. Embedding should be
    // different from ownership.

    //delete mWidget;
    if( mGraphicsScene )
    {
        delete mGraphicsScene;
        mGraphicsScene = 0;
    }

    if( mGraphicsView )
    {
        delete mGraphicsView;
        mGraphicsView = 0;
    }

    if( mGraphicsProxyWidget )
    {
        delete mGraphicsProxyWidget;
        mGraphicsProxyWidget = 0;
    }

    { // Enter critical section
        QMutexLocker locker( mImageMutex );
        if( mImage )
        {
            delete mImage;
            mImage = 0;
        }

        if( mImageFlipped )
        {
            delete mImageFlipped;
            mImageFlipped = 0;
        }
    } // Leave critical section
}
////////////////////////////////////////////////////////////////////////////////
void UIElementQt::_render()
{
    _debug( "_render" );
    // If there's no widget to render, return NULL
    if( mWidget == NULL )
    {
        return;
    }

    /*std::cout << "UIElementQt::_render: Widget(" << mWidget->width() << ", " << mWidget->height()
                << ") Titlebar(" << mTitlebar->width() << ", " << mTitlebar->height()
                << ") Overall(" << mWidth << ", " << mHeight << ") Image("
            << mImageWidth << ", " << mImageHeight << ")" << std::endl << std::flush;*/

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
            m_imagePainter.end();
        }
    } // Leave critical section

    mImageDirty = true;
}
////////////////////////////////////////////////////////////////////////////////
void UIElementQt::paintEvent( QPaintEvent* event )
{
    _debug( "paintEvent" );
    _render();
    QGraphicsView::paintEvent( event );
}
////////////////////////////////////////////////////////////////////////////////
void UIElementQt::_buttonPressEvent( gadget::Keys button, int x, int y, int state )
{
    _buttonEvent( 1, button, x, y, state );
}
////////////////////////////////////////////////////////////////////////////////
void UIElementQt::_buttonReleaseEvent( gadget::Keys button, int x, int y, int state )
{
    _buttonEvent( 0, button, x, y, state );
}
////////////////////////////////////////////////////////////////////////////////
void UIElementQt::_doubleClickEvent( gadget::Keys button, int x, int y, int state )
{
    QWidget *vw = this->childAt( x, y );

    if( vw == NULL ) return;

    Qt::MouseButton qbutton = _extractButton( button );
    Qt::MouseButtons buttons = _extractButtons( state );
    Qt::KeyboardModifiers modifiers = _extractModifiers( state );

    QPoint position( x, y );

    QPoint globalPos = this->viewport()->mapToGlobal( position );

    // Gadgeteer doesn't put buttons into the state except on move events, but
    // Qt expects the button mask to contain all pressed buttons even on
    // press events
    buttons = buttons | qbutton;
    QMouseEvent e( QEvent::MouseButtonDblClick, position, globalPos, qbutton,
                buttons, modifiers );
    qt_sendSpontaneousEvent( this->viewport(), &e );

}
////////////////////////////////////////////////////////////////////////////////
void UIElementQt::_buttonEvent( int type, gadget::Keys button, int x, int y, int state )
{
    QWidget *vw = this->childAt( x, y );

    if( vw == NULL ) return;

    Qt::MouseButton qbutton = _extractButton( button );
    Qt::MouseButtons buttons = _extractButtons( state );
    Qt::KeyboardModifiers modifiers = _extractModifiers( state );

    QPoint position( x, y );

    QPoint globalPos = this->viewport()->mapToGlobal( position );

    if( type == 1 )
    {
        // Gadgeteer doesn't put buttons into the state except on move events, but
        // Qt expects the button mask to contain all pressed buttons even on
        // press events
        buttons = buttons | qbutton;

        QMouseEvent e( QEvent::MouseButtonPress, position, globalPos, qbutton,
                    buttons, modifiers );
        qt_sendSpontaneousEvent( this->viewport(), &e );
    }
    else
    {
        QMouseEvent e( QEvent::MouseButtonRelease, position, globalPos, qbutton,
                    buttons, modifiers );
        qt_sendSpontaneousEvent( this->viewport(), &e );
    }
}
////////////////////////////////////////////////////////////////////////////////
void UIElementQt::_mouseMoveEvent( int x, int y, int z, int state )
{
    Qt::MouseButtons buttons = _extractButtons( state );
    Qt::KeyboardModifiers modifiers = _extractModifiers( state );

    QPoint position( x, y );

    QPoint globalPos = this->viewport()->mapToGlobal( position );

    QMouseEvent e( QEvent::MouseMove, position, globalPos, Qt::NoButton,
                    buttons, modifiers );

    qt_sendSpontaneousEvent( this->viewport(), &e );
}
////////////////////////////////////////////////////////////////////////////////
void UIElementQt::_keyPressEvent( gadget::Keys key, int modifierMask, char unicode )
{
    //Right now VR Juggler has the API to support wide body chars but it 
    //is not wired up. When it is we can use this code to transfer wide body
    //chars to Qt.    
    Qt::KeyboardModifiers modifiers = _extractModifiers( modifierMask );
    QKeyEvent e( QEvent::KeyPress, mKeyMap[key], modifiers, QChar( unicode ) );
    qt_sendSpontaneousEvent( this->viewport(), &e );
}
////////////////////////////////////////////////////////////////////////////////
void UIElementQt::_keyReleaseEvent( gadget::Keys key, int modifierMask, char unicode )
{
    Qt::KeyboardModifiers modifiers = _extractModifiers( modifierMask );
    QKeyEvent e( QEvent::KeyRelease, mKeyMap[key], modifiers, QChar( unicode ) );
    qt_sendSpontaneousEvent( this->viewport(), &e );
}
////////////////////////////////////////////////////////////////////////////////
Qt::MouseButton UIElementQt::_extractButton( gadget::Keys button )
{
    if( button & gadget::MBUTTON1 )
    {
        return Qt::LeftButton;
    }
    else if( button & gadget::MBUTTON2 )
    {
        return Qt::MidButton;
    }
    else if( button & gadget::MBUTTON3 )
    {
        return Qt::RightButton;
    }
    else
    {
        return Qt::NoButton;
    }
}
////////////////////////////////////////////////////////////////////////////////
Qt::MouseButtons UIElementQt::_extractButtons( int state )
{
    Qt::MouseButtons buttons;

    if( state & gadget::BUTTON1_MASK )
    {
        buttons = buttons | Qt::LeftButton;
    }

    if( state & gadget::BUTTON2_MASK )
    {
        buttons = buttons | Qt::MidButton;
    }

    if( state & gadget::BUTTON3_MASK )
    {
        buttons = buttons | Qt::RightButton;
    }

    if( state == 0 )
    {
        buttons = Qt::NoButton;
    }

    return buttons;
}
////////////////////////////////////////////////////////////////////////////////
Qt::KeyboardModifiers UIElementQt::_extractModifiers( int state )
{
    Qt::KeyboardModifiers modifiers;

    if( state & gadget::SHIFT_MASK )
    {
        modifiers = Qt::KeyboardModifier(modifiers | Qt::SHIFT);
    }

    if( state & gadget::ALT_MASK )
    {
        modifiers = Qt::KeyboardModifier(modifiers | Qt::ALT);
    }

    if( state & gadget::CTRL_MASK )
    {
        modifiers = Qt::KeyboardModifier(modifiers | Qt::CTRL);
    }

    if( state & gadget::COMMAND_MASK )
    {
        modifiers = Qt::KeyboardModifier(modifiers | Qt::META);
    }

    return modifiers;
}
////////////////////////////////////////////////////////////////////////////////
void UIElementQt::_setupKeyMap()
{
    mKeyMap[ gadget::KEY_NONE ] = 0;
    mKeyMap[ gadget::KEY_UP ] = Qt::Key_Up;
    mKeyMap[ gadget::KEY_DOWN ] = Qt::Key_Down;
    mKeyMap[ gadget::KEY_LEFT ] = Qt::Key_Left;
    mKeyMap[ gadget::KEY_RIGHT ] = Qt::Key_Right;
    mKeyMap[ gadget::KEY_SHIFT ] = Qt::Key_Shift;
    mKeyMap[ gadget::KEY_CTRL ] = Qt::Key_Control;
    mKeyMap[ gadget::KEY_ALT ] = Qt::Key_Alt;
    mKeyMap[ gadget::KEY_COMMAND ] = Qt::Key_Control;
    mKeyMap[ gadget::KEY_1 ] = Qt::Key_1;
    mKeyMap[ gadget::KEY_2 ] = Qt::Key_2;
    mKeyMap[ gadget::KEY_3 ] = Qt::Key_3;
    mKeyMap[ gadget::KEY_4 ] = Qt::Key_4;
    mKeyMap[ gadget::KEY_5 ] = Qt::Key_5;
    mKeyMap[ gadget::KEY_6 ] = Qt::Key_6;
    mKeyMap[ gadget::KEY_7 ] = Qt::Key_7;
    mKeyMap[ gadget::KEY_8 ] = Qt::Key_8;
    mKeyMap[ gadget::KEY_9 ] = Qt::Key_9;
    mKeyMap[ gadget::KEY_0 ] = Qt::Key_0;
    mKeyMap[ gadget::KEY_A ] = Qt::Key_A;
    mKeyMap[ gadget::KEY_B ] = Qt::Key_B;
    mKeyMap[ gadget::KEY_C ] = Qt::Key_C;
    mKeyMap[ gadget::KEY_D ] = Qt::Key_D;
    mKeyMap[ gadget::KEY_E ] = Qt::Key_E;
    mKeyMap[ gadget::KEY_F ] = Qt::Key_F;
    mKeyMap[ gadget::KEY_G ] = Qt::Key_G;
    mKeyMap[ gadget::KEY_H ] = Qt::Key_H;
    mKeyMap[ gadget::KEY_I ] = Qt::Key_I;
    mKeyMap[ gadget::KEY_J ] = Qt::Key_J;
    mKeyMap[ gadget::KEY_K ] = Qt::Key_K;
    mKeyMap[ gadget::KEY_L ] = Qt::Key_L;
    mKeyMap[ gadget::KEY_M ] = Qt::Key_M;
    mKeyMap[ gadget::KEY_N ] = Qt::Key_N;
    mKeyMap[ gadget::KEY_O ] = Qt::Key_O;
    mKeyMap[ gadget::KEY_P ] = Qt::Key_P;
    mKeyMap[ gadget::KEY_Q ] = Qt::Key_Q;
    mKeyMap[ gadget::KEY_R ] = Qt::Key_R;
    mKeyMap[ gadget::KEY_S ] = Qt::Key_S;
    mKeyMap[ gadget::KEY_T ] = Qt::Key_T;
    mKeyMap[ gadget::KEY_U ] = Qt::Key_U;
    mKeyMap[ gadget::KEY_V ] = Qt::Key_V;
    mKeyMap[ gadget::KEY_W ] = Qt::Key_W;
    mKeyMap[ gadget::KEY_X ] = Qt::Key_X;
    mKeyMap[ gadget::KEY_Y ] = Qt::Key_Y;
    mKeyMap[ gadget::KEY_Z ] = Qt::Key_Z;
    mKeyMap[ gadget::KEY_ESC ] = Qt::Key_Escape;
    mKeyMap[ gadget::KEY_TAB ] = Qt::Key_Tab;
    mKeyMap[ gadget::KEY_BACKTAB ] = Qt::Key_Backtab;
    mKeyMap[ gadget::KEY_BACKSPACE ] = Qt::Key_Backspace;
    mKeyMap[ gadget::KEY_RETURN ] = Qt::Key_Return;
    mKeyMap[ gadget::KEY_ENTER ] = Qt::Key_Enter;
    mKeyMap[ gadget::KEY_INSERT ] = Qt::Key_Insert;
    mKeyMap[ gadget::KEY_DELETE ] = Qt::Key_Delete;
    mKeyMap[ gadget::KEY_PAUSE ] = Qt::Key_Pause;
    mKeyMap[ gadget::KEY_PRINT ] = Qt::Key_Print;
    mKeyMap[ gadget::KEY_SYSREQ ] = Qt::Key_SysReq;
    mKeyMap[ gadget::KEY_HOME ] = Qt::Key_Home;
    mKeyMap[ gadget::KEY_END ] = Qt::Key_End;
    mKeyMap[ gadget::KEY_PRIOR ] = Qt::Key_PageUp;
    mKeyMap[ gadget::KEY_NEXT ] = Qt::Key_PageDown;
    mKeyMap[ gadget::KEY_CAPS_LOCK ] = Qt::Key_CapsLock;
    mKeyMap[ gadget::KEY_NUM_LOCK ] = Qt::Key_NumLock;
    mKeyMap[ gadget::KEY_SCROLL_LOCK ] = Qt::Key_ScrollLock;
    mKeyMap[ gadget::KEY_F1 ] = Qt::Key_F1;
    mKeyMap[ gadget::KEY_F2 ] = Qt::Key_F2;
    mKeyMap[ gadget::KEY_F3 ] = Qt::Key_F3;
    mKeyMap[ gadget::KEY_F4 ] = Qt::Key_F4;
    mKeyMap[ gadget::KEY_F5 ] = Qt::Key_F5;
    mKeyMap[ gadget::KEY_F6 ] = Qt::Key_F6;
    mKeyMap[ gadget::KEY_F7 ] = Qt::Key_F7;
    mKeyMap[ gadget::KEY_F8 ] = Qt::Key_F8;
    mKeyMap[ gadget::KEY_F9 ] = Qt::Key_F9;
    mKeyMap[ gadget::KEY_F10 ] = Qt::Key_F10;
    mKeyMap[ gadget::KEY_F11 ] = Qt::Key_F11;
    mKeyMap[ gadget::KEY_F12 ] = Qt::Key_F12;
    mKeyMap[ gadget::KEY_F13 ] = Qt::Key_F13;
    mKeyMap[ gadget::KEY_F14 ] = Qt::Key_F14;
    mKeyMap[ gadget::KEY_F15 ] = Qt::Key_F15;
    mKeyMap[ gadget::KEY_F16 ] = Qt::Key_F16;
    mKeyMap[ gadget::KEY_F17 ] = Qt::Key_F17;
    mKeyMap[ gadget::KEY_F18 ] = Qt::Key_F18;
    mKeyMap[ gadget::KEY_F19 ] = Qt::Key_F19;
    mKeyMap[ gadget::KEY_F20 ] = Qt::Key_F20;
    mKeyMap[ gadget::KEY_F21 ] = Qt::Key_F21;
    mKeyMap[ gadget::KEY_F22 ] = Qt::Key_F22;
    mKeyMap[ gadget::KEY_F23 ] = Qt::Key_F23;
    mKeyMap[ gadget::KEY_F24 ] = Qt::Key_F24;
    mKeyMap[ gadget::KEY_F25 ] = Qt::Key_F25;
    mKeyMap[ gadget::KEY_F26 ] = Qt::Key_F26;
    mKeyMap[ gadget::KEY_F27 ] = Qt::Key_F27;
    mKeyMap[ gadget::KEY_F28 ] = Qt::Key_F28;
    mKeyMap[ gadget::KEY_F29 ] = Qt::Key_F29;
    mKeyMap[ gadget::KEY_F30 ] = Qt::Key_F30;
    mKeyMap[ gadget::KEY_F31 ] = Qt::Key_F31;
    mKeyMap[ gadget::KEY_F32 ] = Qt::Key_F32;
    mKeyMap[ gadget::KEY_F33 ] = Qt::Key_F33;
    mKeyMap[ gadget::KEY_F34 ] = Qt::Key_F34;
    mKeyMap[ gadget::KEY_F35 ] = Qt::Key_F35;
    mKeyMap[ gadget::KEY_SUPER_L ] = Qt::Key_Super_L;
    mKeyMap[ gadget::KEY_SUPER_R ] = Qt::Key_Super_R;
    mKeyMap[ gadget::KEY_MENU ] = Qt::Key_Menu;
    mKeyMap[ gadget::KEY_HYPER_L ] = Qt::Key_Hyper_L;
    mKeyMap[ gadget::KEY_HYPER_R ] = Qt::Key_Hyper_R;
    mKeyMap[ gadget::KEY_HELP ] = Qt::Key_Help;
    mKeyMap[ gadget::KEY_SPACE ] = Qt::Key_Space;
    mKeyMap[ gadget::KEY_ANY ] = Qt::Key_Any;
    mKeyMap[ gadget::KEY_EXCLAM ] = Qt::Key_Exclam;
    mKeyMap[ gadget::KEY_QUOTE_DBL ] = Qt::Key_QuoteDbl;
    mKeyMap[ gadget::KEY_NUMBER_SIGN ] = Qt::Key_NumberSign;
    mKeyMap[ gadget::KEY_DOLLAR ] = Qt::Key_Dollar;
    mKeyMap[ gadget::KEY_PERCENT ] = Qt::Key_Percent;
    mKeyMap[ gadget::KEY_AMPERSAND ] = Qt::Key_Ampersand;
    mKeyMap[ gadget::KEY_APOSTROPHE ] = Qt::Key_Apostrophe;
    mKeyMap[ gadget::KEY_PAREN_LEFT ] = Qt::Key_ParenLeft;
    mKeyMap[ gadget::KEY_PAREN_RIGHT ] = Qt::Key_ParenRight;
    mKeyMap[ gadget::KEY_ASTERISK ] = Qt::Key_Asterisk;
    mKeyMap[ gadget::KEY_PLUS ] = Qt::Key_Plus;
    mKeyMap[ gadget::KEY_COMMA ] = Qt::Key_Comma;
    mKeyMap[ gadget::KEY_MINUS ] = Qt::Key_Minus;
    mKeyMap[ gadget::KEY_PERIOD ] = Qt::Key_Period;
    mKeyMap[ gadget::KEY_SLASH ] = Qt::Key_Slash;
    mKeyMap[ gadget::KEY_COLON ] = Qt::Key_Colon;
    mKeyMap[ gadget::KEY_SEMICOLON ] = Qt::Key_Semicolon;
    mKeyMap[ gadget::KEY_LESS ] = Qt::Key_Less;
    mKeyMap[ gadget::KEY_EQUAL ] = Qt::Key_Equal;
    mKeyMap[ gadget::KEY_GREATER ] = Qt::Key_Greater;
    mKeyMap[ gadget::KEY_QUESTION ] = Qt::Key_Question;
    mKeyMap[ gadget::KEY_AT ] = Qt::Key_At;
    mKeyMap[ gadget::KEY_BRACKET_LEFT ] = Qt::Key_BracketLeft;
    mKeyMap[ gadget::KEY_BACKSLASH ] = Qt::Key_Backslash;
    mKeyMap[ gadget::KEY_BRACKET_RIGHT ] = Qt::Key_BracketRight;
    mKeyMap[ gadget::KEY_ASCII_CIRCUM ] = Qt::Key_AsciiCircum;
    mKeyMap[ gadget::KEY_UNDERSCORE ] = Qt::Key_Underscore;
    mKeyMap[ gadget::KEY_QUOTE_LEFT ] = Qt::Key_QuoteLeft;
    mKeyMap[ gadget::KEY_BRACE_LEFT ] = Qt::Key_BraceLeft;
    mKeyMap[ gadget::KEY_BAR ] = Qt::Key_Bar;
    mKeyMap[ gadget::KEY_BRACE_RIGHT ] = Qt::Key_BraceRight;
    mKeyMap[ gadget::KEY_ASCII_TILDE ] = Qt::Key_AsciiTilde;
    mKeyMap[ gadget::KEY_UNKNOWN ] = Qt::Key_unknown;
}
////////////////////////////////////////////////////////////////////////////////
void UIElementQt::_onHideButtonClicked()
{
    ves::conductor::UIManager::instance()->HideElement( this );
}
////////////////////////////////////////////////////////////////////////////////
void UIElementQt::_onMinimizeButtonClicked()
{
    ves::conductor::UIManager::instance()->MinimizeElement( this );
}
////////////////////////////////////////////////////////////////////////////////
void UIElementQt::_onTitlebarPressed()
{
    ves::conductor::UIManager::instance()->InitiateMoveElement( this );
}
////////////////////////////////////////////////////////////////////////////////
void UIElementQt::_onOpacitySliderValueChanged( int opacity )
{
    ves::conductor::UIManager::instance()->SetOpacity( opacity/100.0f );
}
////////////////////////////////////////////////////////////////////////////////
} // namespace conductor
} // namespace ves
