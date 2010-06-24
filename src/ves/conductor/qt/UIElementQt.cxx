/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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

#include <gadget/Type/KeyboardMouse/Keys.h>

// --- VES includes --- //
#include <ves/conductor/qt/UIElementQt.h>
#include <ves/xplorer/eventmanager/InteractionEvent.h>

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

//#define _debug(text) std::cout << "UIElementQt::" << text << std::endl << std::flush
#define _debug(text)

// qt_sendSpontaneousEvent is a friend method of QCoreApplication and thus
// gives us access to the private method sendSpontaneousEvent, which is required
// to propagate mouse and keyboard events to the off-screen Qt element as though
// the events came directly from the window manager. Event propagation will
// not work correctly without this.

bool qt_sendSpontaneousEvent( QObject* recv, QEvent* e )
{
    return QCoreApplication::sendSpontaneousEvent( recv, e );
}

namespace ves
{
namespace conductor
{

UIElementQt::UIElementQt( QWidget *parent ) : QGraphicsView( parent ),
mWidget( NULL ),
    mImage( NULL ),
    mImageFlipped( NULL ),
    mGraphicsScene( NULL ),
    mGraphicsProxyWidget( NULL ),
    mGraphicsView( NULL ),
    mTimer( NULL ),
    mImageWidth( 0 ),
    mImageHeight( 0 ),
    mWidth( 0 ),
    mHeight( 0 ),
    mTextureLeft( 0.0f ),
    mTextureRight( 0.0f ),
    mTextureBottom( 0.0f ),
    mTextureTop( 0.0f ),
    mInitialized( false ),
    mImageDirty( true ),
    mDirty( true )
{
    _debug( "ctor" );
    this->setCacheMode( QGraphicsView::CacheNone );
    setRenderHints( QPainter::NonCosmeticDefaultPen );
    QBrush brush( QColor( 0, 0, 0, 0 ) );
    brush.setStyle( Qt::SolidPattern );
    this->setBackgroundBrush( brush );
    mImageMutex = new QMutex( );
    _setupKeyMap();
    Initialize( );
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
        QObject::connect( this, SIGNAL( PutSendEvent( ves::xplorer::eventmanager::InteractionEvent* ) ),
            this, SLOT( _sendEvent( ves::xplorer::eventmanager::InteractionEvent* ) ), Qt::QueuedConnection );
        QObject::connect( this, SIGNAL( RequestEmbed( bool ) ), this, SLOT( _embed( bool ) ), Qt::QueuedConnection );

        // Start up the timer that causes repaints at a set interval -- assuming
        // thread is given execution sometime during this interval.
        mTimer = new QTimer( this );
        QObject::connect( mTimer, SIGNAL( timeout( ) ), this, SLOT( _render( ) ) );
        mTimer->start( 100 );

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

void UIElementQt::SendInteractionEvent( ves::xplorer::eventmanager::InteractionEvent&
                                        event )
{
    ves::xplorer::eventmanager::InteractionEvent* m_event =
        new ves::xplorer::eventmanager::InteractionEvent( event );

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
    // that causes a repaint of this element every 100ms. Any time the present
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

bool UIElementQt::IsDirty( )
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
    // tab-focus feedback and odd cursor behavior on Qt 4.6.x. Unfortunately,
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
    if( mGraphicsView )
    {
        delete mGraphicsView;
    }
    
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
    mImageWidth = int( pow( 2, ceil( log( double(mWidth) ) / log( double(2) ) ) ) );
    mImageHeight = int( pow( 2, ceil( log( double(mHeight) ) / log( double(2) ) ) ) );

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

void UIElementQt::paintEvent( QPaintEvent* event )
{
    _debug( "paintEvent" );
    _render( );
    QGraphicsView::paintEvent( event );
}

void UIElementQt::_sendEvent( ves::xplorer::eventmanager::InteractionEvent* event )
{
    using ves::xplorer::eventmanager::InteractionEvent;
    int x = static_cast < int > ( event->X );
    int y = static_cast < int > ( event->Y );

    QPoint globalPos = this->viewport( )->mapToGlobal( QPoint( x, y ) );
    QWidget *vw = this->childAt( x, y );

    if( ( event->EventType != InteractionEvent::keyPress ) && ( event->EventType != InteractionEvent::keyRelease ) )
    {
        if( vw == NULL ) return;
    }

    // keyPress, keyRelease, scroll
    // This is incomplete.
    // 1. Does not deal with scroll events
    // 2. Always assumes left mouse button and never actually looks at which button.
    // 3. Does not look at modifier keys.
    switch( event->EventType )
    {
    case InteractionEvent::buttonPress:
    {
        QMouseEvent e( QEvent::MouseButtonPress, QPoint( x, y ), globalPos, Qt::LeftButton,
                       Qt::LeftButton, Qt::NoModifier );
        qt_sendSpontaneousEvent( this->viewport( ), &e );
    }
        break;
    case InteractionEvent::buttonRelease:
    {
        QMouseEvent e( QEvent::MouseButtonRelease, QPoint( x, y ), globalPos, Qt::LeftButton,
                       Qt::LeftButton, Qt::NoModifier );
        qt_sendSpontaneousEvent( this->viewport( ), &e );
    }
        break;
    case InteractionEvent::pointerMotion:
    {
        if( event->Buttons == InteractionEvent::button_none )
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
    case InteractionEvent::keyPress:
    {
        wchar_t uniKey = event->KeyUnicode;
        QString qUniKey = QString::fromWCharArray( &uniKey, 1 );
        QKeyEvent e( QEvent::KeyPress, mKeyMap[event->Key], Qt::NoModifier, qUniKey );
        qt_sendSpontaneousEvent( this->viewport( ), &e );
    }
        break;
    case InteractionEvent::keyRelease:
    {
        wchar_t uniKey = event->KeyUnicode;
        QString qUniKey = QString::fromWCharArray( &uniKey, 1 );
        QKeyEvent e( QEvent::KeyRelease, mKeyMap[event->Key], Qt::NoModifier, qUniKey );
        qt_sendSpontaneousEvent( this->viewport( ), &e );;
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

void UIElementQt::_setupKeyMap( )
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

} // namespace conductor
} // namespace ves
