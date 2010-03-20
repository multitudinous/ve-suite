#ifndef UIELEMENTQT_H
#define UIELEMENTQT_H

#define QT_NO_KEYWORDS

// Base class header
#include <ves/conductor/qt/UIElement.h>


#include <QtCore/QObject>
#include <QtGui/QWidget>
#include <QtGui/QMouseEvent>
#include <QtGui/QGraphicsScene>
#include <QtGui/QGraphicsView>
#include <QtGui/QGraphicsProxyWidget>
#include <QtCore/QTimer>
#include <QtCore/QMutex>

namespace ves
{
namespace xplorer
{
namespace util
{
class InteractionEvent;
} // namespace util
} // namespace xplorer
namespace conductor
{
/// @file UIElementQt.h

/// @class ves::conductor::UIElementQt
/// Wraps Qt widgets for use as UIElements in UIManager. Takes in a normal
/// Qt widget and converts it to use off-screen rendering to render to a 
/// texture. This class also propagates mouse and keyboard events down into the
/// widget so that it behaves as though it is being displayed on-screen.
class UIElementQt :  public QGraphicsView, public UIElement
{

    Q_OBJECT

public:
    UIElementQt( QWidget *parent = 0 );
    virtual ~UIElementQt( );

    // Required overrides from UIElement; see base class for documentation
    virtual int GetImageWidth( );
    virtual int GetImageHeight( );
    virtual int GetElementWidth( );
    virtual int GetElementHeight( );
    const virtual osg::Vec4f GetTextureCoordinates( );
    virtual void SendInteractionEvent( xplorer::util::InteractionEvent &event );
    virtual unsigned char* RenderElementToImage( );
    virtual bool IsDirty();
    virtual void Initialize( );
    virtual void Unembed( );
    virtual void Embed();

    // Functions unique to this derived class
    ////////////////////////////////////////////////////////////////////////////////
    /// Sets the Qt widget associated with this element
    /// UIElementQt takes ownership of the widget and is responsible for its
    /// eventual destruction. DO NOT CALL widget's destructor externally after you
    /// have passed it to SetWidget!
    ///
    /// @param widget The Qt widget to associate with this element
    ////////////////////////////////////////////////////////////////////////////////
    void SetWidget( QWidget* widget );

    ////////////////////////////////////////////////////////////////////////////////
    /// Tell this element that its size has changed.
    /// The widget passed in SetWidget should call this function anytime its size
    /// has changed. Calling this function from inside a QWidget::resizeEvent is
    /// usually the recommended route.
    ////////////////////////////////////////////////////////////////////////////////
    void UpdateSize( );
    
protected:
    void paintEvent ( QPaintEvent * event );

Q_SIGNALS:
    void RequestRender( );
    void PutSendEvent( xplorer::util::InteractionEvent *event );
    void RequestEmbed( bool embed );

private:
    void FreeOldWidgets( );
    QWidget *mWidget; /// Widget associated with this element
    QImage *mImage; /// Rendered image of element
    QImage *mImageFlipped; /// Vertically flipped image of element
    QGraphicsScene *mGraphicsScene; /// Scene to hold widget
    QGraphicsProxyWidget *mGraphicsProxyWidget; /// Proxy widget that handles internal event forwarding
    QGraphicsView *mGraphicsView; /// View to display scene and allow interaction
    int mImageWidth; /// Width of rendered image
    int mImageHeight; /// Height of rendered image
    int mWidth; /// Width of widget
    int mHeight; /// Height of widget
    float mTextureLeft; /// Left-most texture coordinate, on interval [0,1]
    float mTextureRight; /// Right-most texture coordinate, on interval [0,1]
    float mTextureBottom; /// Bottom-most texture coordinate, on interval [0,1]
    float mTextureTop; /// Top-most texture coordinate, on interval [0,1]
    bool mInitialized; /// Flag telling whether this element has been initialized
    bool mImageDirty; /// Flag telling whether the rendered image has changed
    QTimer *mTimer; /// Timer that causes UI to render at set intervals
    QMutex *mImageMutex; /// Mutex that is used to avoid access collisions on rendered image
    bool mDirty; /// Flag telling outside world whether image has changed; holds
                 /// slightly different state from mImageDirty

    void _calculatePower2ImageDimensions( );
    void _calculateTextureCoordinates( );
    void _debug( const std::string text );

protected Q_SLOTS:
    void _render( );
    void _sendEvent( xplorer::util::InteractionEvent *event );
    void _embed( bool embed );

};

} // namespace conductor
} // namespace ves

#endif // UIELEMENTQT_H
