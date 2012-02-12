/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
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
#pragma once

#define QT_NO_KEYWORDS

// Base class header
#include <ves/VEConfig.h>

#include <ves/xplorer/Logging.h>

#include <ves/conductor/qt/UIElement.h>
#include <ves/conductor/qt/NonScrollGraphicsView.h>

#include <QtCore/QObject>
#include <QtGui/QWidget>
#include <QtGui/QMouseEvent>
#include <QtGui/QGraphicsScene>
#include <QtGui/QGraphicsView>
#include <QtGui/QGraphicsProxyWidget>
#include <QtCore/QTimer>
#include <QtCore/QMutex>
#include <QtCore/qobjectdefs.h>
#include <QtGui/QLabel>
#include <QtCore/QTime>

namespace Ui {
    class titlebar;
}

namespace ves
{
namespace conductor
{
/// @file UIElementQt.h

/// @class ves::conductor::UIElementQt
/// Wraps Qt widgets for use as UIElements in UIManager. Takes in a normal
/// Qt widget and converts it to use off-screen rendering to render to a 
/// texture. This class also propagates mouse and keyboard events down into the
/// widget so that it behaves as though it is being displayed on-screen.

class VE_CONDUCTOR_QTUI_EXPORTS UIElementQt : public NonScrollGraphicsView, public UIElement
{
    Q_OBJECT

public:
    UIElementQt( QWidget* parent = 0 );
    virtual ~UIElementQt();

    // Required overrides from UIElement; see base class for documentation
    virtual int GetImageWidth();
    virtual int GetImageHeight();
    virtual int GetElementWidth();
    virtual int GetElementHeight();
    const virtual osg::Vec4f GetTextureCoordinates();
    virtual void SendInteractionEvent( ves::xplorer::eventmanager::InteractionEvent &event );

    virtual void SendButtonPressEvent( gadget::Keys button, int x, int y, int state );
    virtual void SendButtonReleaseEvent( gadget::Keys button, int x, int y, int state );
    virtual void SendDoubleClickEvent( gadget::Keys button, int x, int y, int state );
    virtual void SendMouseMoveEvent( int x, int y, int z, int state );
    virtual void SendKeyPressEvent( gadget::Keys key, int modifierMask, char unicode );
    virtual void SendKeyReleaseEvent( gadget::Keys key, int modifierMask, char unicode );
    virtual void SendScrollEvent( int deltaX, int deltaY, int x, int y, int state );

    virtual osg::Image* RenderElementToImage();
    virtual bool IsDirty();
    virtual void Initialize();
    virtual std::vector< std::pair< osg::ref_ptr<osg::Image>, std::pair< int, int > > > const& GetDamagedAreas();

    ///Override this so that we can dirty the image and ask for a draw
    //virtual void MoveCanvas( float dx, float dy, float dz );

    // Functions unique to this derived class

    /**
     * Sets the Qt widget associated with this element
     * UIElementQt takes ownership of the widget and is responsible for its
     * eventual destruction. DO NOT CALL widget's destructor externally after you
     * have passed it to SetWidget!
     *
     * @param widget Pointer to the Qt widget to associate with this element
     **/
    void SetWidget( QWidget* widget );

    ////////////////////////////////////////////////////////////////////////////////
    /// Tell this element that its size has changed.
    /// The widget passed in SetWidget should call this function anytime its size
    /// has changed. Calling this function from inside a QWidget::resizeEvent is
    /// usually the recommended route.
    ////////////////////////////////////////////////////////////////////////////////
    void UpdateSize();

    virtual void ResizeCanvas( int width, int height );

    virtual void Raise();

    virtual void Lower();

    virtual void ShowTitlebar( bool show );

    virtual void ToggleTitlebar();

    virtual bool SizeDirty();

protected:
    bool eventFilter(QObject *object, QEvent *event);

private:
    void FreeOldWidgets();
    QWidget* mWidget; ///< Widget associated with this element
    QImage* mImage; ///< Rendered image of element
    QImage* mImageFlipped; ///< Vertically flipped image of element
    QGraphicsScene* mGraphicsScene; ///< Scene to hold widget
    QGraphicsProxyWidget* mGraphicsProxyWidget; ///< Proxy widget that handles internal event forwarding
    QGraphicsView* mGraphicsView; ///< View to display scene and allow interaction
    int mImageWidth; ///< Width of rendered image
    int mImageHeight; ///< Height of rendered image
    int mWidth; ///< Width of widget
    int mHeight; ///< Height of widget
    float mTextureLeft; ///< Left-most texture coordinate, on interval [0,1]
    float mTextureRight; ///< Right-most texture coordinate, on interval [0,1]
    float mTextureBottom; ///< Bottom-most texture coordinate, on interval [0,1]
    float mTextureTop; ///< Top-most texture coordinate, on interval [0,1]
    bool mInitialized; ///< Flag telling whether this element has been initialized
    bool mImageDirty; ///< Flag telling whether the rendered image has changed
    QTimer* mTimer; ///< Timer that causes UI to render at set intervals
    QMutex* mImageMutex; ///< Mutex that is used to avoid access collisions on rendered image
    bool mDirty; ///< Flag telling outside world whether image has changed; holds
    ///< slightly different state from mImageDirty
    std::map<int,int> mKeyMap; ///< Map to convert juggler keycodes to Qt keycodes

    void _buttonEvent( int type, gadget::Keys button, int x, int y, int state );
    Qt::MouseButton _extractButton( gadget::Keys button );
    Qt::MouseButtons _extractButtons( int state );
    Qt::KeyboardModifiers _extractModifiers( int state );
    void _setupKeyMap();

    QWidget* mTitlebar;
    Ui::titlebar* mQTitlebar;
    QMap<QString, QImage>   update_cache;
    QMap<QString, QRect>  m_captureList;
    QMutex* m_captureListMutex;
    bool m_firstRender;
    bool m_sizeHasChanged;
    bool m_sizeDirty;

#ifdef FULL_IMAGE_DEBUG
    QLabel* m_imgCurrentLabel;
    QLabel* m_imgPreviousLabel;
#endif

    QTime m_paintTime;

protected Q_SLOTS:
    void _render();
    //void _sendEvent( ves::xplorer::eventmanager::InteractionEvent* event );
    void _resizeCanvas( int width, int height );

    void _buttonPressEvent( gadget::Keys button, int x, int y, int state );
    void _buttonReleaseEvent( gadget::Keys button, int x, int y, int state );
    void _doubleClickEvent( gadget::Keys button, int x, int y, int state );
    void _mouseMoveEvent( int x, int y, int z, int state );
    void _keyPressEvent( gadget::Keys key, int modifierMask, char unicode );
    void _keyReleaseEvent( gadget::Keys key, int modifierMask, char unicode );
    void _scrollEvent( int deltaX, int deltaY, int x, int y, int state );

    void _onHideButtonClicked();
    void _onMinimizeButtonClicked();
    void _onTitlebarPressed();
    void _onOpacitySliderValueChanged( int opacity );
    void RefreshWidgetFilterList();
    void _raise();
    void _lower();

    Q_SIGNALS:
    void RequestRender();
    void PutResizeCanvas( int width, int height );
    //void PutSendEvent( ves::xplorer::eventmanager::InteractionEvent* event );
    void PutButtonPressEvent( gadget::Keys button, int x, int y, int state );
    void PutButtonReleaseEvent( gadget::Keys button, int x, int y, int state );
    void PutDoubleClickEvent( gadget::Keys button, int x, int y, int state );
    void PutMouseMoveEvent( int x, int y, int z, int state );
    void PutKeyPressEvent( gadget::Keys key, int modifierMask, char unicode );
    void PutKeyReleaseEvent( gadget::Keys key, int modifierMask, char unicode );
    void PutScrollEvent( int deltaX, int deltaY, int x, int y, int state );
    void PutRaise();
    void PutLower();

protected:
    ///Logger reference
    Poco::Logger& m_logger;
    ///Actual stream for this class
    ves::xplorer::LogStreamPtr m_logStream;
};

} // namespace conductor
} // namespace ves
