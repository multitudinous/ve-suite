////////////////////////////////////////////////////////////////////////////////
#include "mglwidget.h"

//#define FB

bool qt_sendSpontaneousEvent( QObject* recv, QEvent* e )
{
    QCoreApplication::sendSpontaneousEvent( recv, e );
}

mGLWidget::mGLWidget( QWidget *parent )
    : QGLWidget(QGLFormat(QGL::SampleBuffers|QGL::AlphaChannel), parent)
{
    // Turn on mouse tracking so that we get mouse move events even when no
    // button is depressed
    this->setMouseTracking( true );

    setWindowTitle(tr("Off-screen rendering into a QGLFramebufferObject"));
    makeCurrent();

    m_dialog = new myDialog( 0 );
    m_dialog->resize( m_dialog->sizeHint() );
    //m_dialog->resize( 300, 300 );
    int dwidth = m_dialog->size().width();
    int dheight = m_dialog->size().height();

    //** Begin(Move) into vesUIElement
    m_scene = new QGraphicsScene( 0, 0, dwidth, dheight );
    m_view = new QGraphicsView( m_scene, 0 );
    m_view->setAlignment(Qt::AlignLeft | Qt::AlignTop);
    m_proxy = m_scene->addWidget( m_dialog );
    m_proxy->show();
    m_dialog->move( 0, 0 );
    m_dialog->setMouseTracking( true );
    m_view->setMouseTracking( true );
    m_view->viewport()->setMouseTracking( true );

    m_view->setSceneRect( m_scene->sceneRect() );
    m_view->setHorizontalScrollBarPolicy( Qt::ScrollBarAlwaysOff );
    m_view->setVerticalScrollBarPolicy( Qt::ScrollBarAlwaysOff );
    m_view->resize( dwidth, dheight );

//    m_scene->setFocus();
//    m_scene->setStickyFocus( true );
//    m_scene->setActiveWindow( m_proxy );
//    m_proxy->setActive( true );
//    m_scene->setActivePanel( m_proxy );
//    m_scene->setFocusItem( m_proxy, Qt::ActiveWindowFocusReason );

    //m_view->show();
    //m_dialog->setFocus( Qt::ActiveWindowFocusReason );
    //m_view->setFocus( Qt::ActiveWindowFocusReason );

    //m_view->show();
    //m_view->raise();
    //m_view->activateWindow();
    //m_view->setAttribute( Qt::WA_DontShowOnScreen );
    //m_dialog->setWindowState(Qt::WindowActive);
    m_dialog->ensurePolished();
    //m_dialog->overrideWindowState(Qt::WindowActive);
    //m_dialog->activateWindow();
    //m_dialog->setFocus();

    //m_dialog->setFocus(Qt::ActiveWindowFocusReason);
    //m_dialog->findChild<QWidget*>()->setFocus();

    // Suggestions from Girish
    {
    QEvent ev(QEvent::WindowActivate);
    //QApplication::sendEvent(m_view, &ev);
    QApplication::sendEvent(m_scene, &ev);
//
//    QEvent ev2(QEvent::ActivationChange);
//    QApplication::sendEvent(m_view, &ev2);
//    QApplication::sendEvent(m_scene, &ev2);

//    m_proxy->setActive( true );
//    m_scene->setFocusItem(m_proxy, Qt::ActiveWindowFocusReason);
    }

    //m_view->show();

    QSize fbSize = calculateTextureSize( dwidth, dheight );

#ifdef FB
    m_frameBuffer = new QGLFramebufferObject( fbSize );
#endif

    m_image = new QImage( fbSize, QImage::Format_ARGB32 );
    //** End(Move)


    // Every 30 ms we force a redraw of the window. This could be accomplished
    // in a number of other ways. The most interesting would be to simply use
    // this timer as a way to update the texture, and allow repaints of the
    // GL context to occur based on some other signal.
    m_timer = new QTimer( this );
    connect( m_timer, SIGNAL( timeout() ), this, SLOT( update() ) );
    m_timer->start( 30 );
}

mGLWidget::~mGLWidget()
{
    //** Begin(Move) into vesUIElement
#ifdef FB
    delete m_frameBuffer;
#endif
    delete m_image;
    if (m_dialog) delete m_dialog;
    if (m_scene) delete m_scene;
    if (m_view) delete m_view;
    //** End(Move)
}

void mGLWidget::initializeGL()
{
}

////////////////////////////////////////////////////////////////////////////////
// paintGL: override of function from base class
// Called whenever the widget receives a paintEvent.
// GLContext is already current when this is called.
////////////////////////////////////////////////////////////////////////////////
void mGLWidget::paintGL()
{
    saveGLState();
    m_dialog->update();

#ifdef FB
    //** Begin(Move) into vesUIElement
    QPainter frameBufferPainter( m_frameBuffer );
    QRect tempRect( 0, 0, int(m_scene->sceneRect().width()), int(m_scene->sceneRect().height()) );
    m_view->render( &frameBufferPainter, tempRect, tempRect );
    frameBufferPainter.end();
    //** End(Move)
#else
    //Begin(Test)
    QPainter m_imagePainter( m_image );
    QRect m_Rectangle( 0, 0, int(m_scene->sceneRect().width()), int(m_scene->sceneRect().height()) );
    m_view->render(&m_imagePainter, m_Rectangle, m_Rectangle);
    m_imagePainter.end();
    QImage image2(m_image->mirrored( false, true ));
    //image2.invertPixels();
    //End(Test)
#endif

    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glOrtho( 0, width(), 0, height(), -1.0f, 1.0f );
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    glViewport(0, 0, width(), height());
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
#ifdef FB
    glBindTexture(GL_TEXTURE_2D, m_frameBuffer->texture());
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
#else
    glBindTexture(GL_TEXTURE_2D, 1);
    glPixelStorei(GL_UNPACK_ALIGNMENT, 4);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
    glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
    glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_DECAL);
    glTexImage2D( GL_TEXTURE_2D, 0, 4, m_image->width(), m_image->height(), 0, GL_BGRA, GL_UNSIGNED_BYTE, image2.bits() );
    glBindTexture (GL_TEXTURE_2D, 1);
#endif


    glEnable(GL_MULTISAMPLE);
    glEnable(GL_CULL_FACE);

    glPushMatrix();

    glBegin(GL_QUADS);
     glColor3f( 1.0f, 0.0f, 0.0f );
     glVertex3f( 0.0f, 0.0f, 0.0f );

     glColor3f( 0.0f, 1.0f, 0.0f );
     glVertex3f( this->width(), 0.0f, 0.0f );

     glColor3f( 0.0f, 0.0f, 1.0f );
     glVertex3f( this->width(), this->height(), 0.0f );

     glVertex3f( 0.0f, this->height(), 0.0f );
   glEnd();

    int mwidth = m_view->width();
    int mheight = m_view->height();

    glRotatef( m_dialog->findChild<QDoubleSpinBox *>("rotationSpinner")->value(), 0.0f, 0.0f, 1.0f );

    float yOffset = this->height() - mheight;

    //** Begin(Move) into vesUIElement
    // Also add textureXMin and textureYMax
    float textureXMax = float(m_view->width()) / float(m_image->size().width());
    float textureYMin = 1.0f - float(m_view->height()) / float(m_image->size().height());
    //** End(Move)

    // Here is how we can set the opacity of the dialog:
    glColor4f( 1.0f, 1.0f, 1.0f,
             m_dialog->findChild<QDoubleSpinBox *>("opacitySpinner")->value() );
    glEnable(GL_TEXTURE_2D);
    glBegin(GL_QUADS);
    {
        glTexCoord2f( 0.0f, textureYMin );
          glVertex3f( 0.0f, 0.0f + yOffset, 0.0f );

        glTexCoord2f( textureXMax, textureYMin );
          glVertex3f( mwidth, 0.0f + yOffset, 0.0f );

        glTexCoord2f( textureXMax, 1.0f );
          glVertex3f( mwidth, mheight + yOffset, 0.0f );

        glTexCoord2f( 0.0f, 1.0f );
          glVertex3f( 0.0f, mheight + yOffset, 0.0f );
    }
    glEnd();
    glDisable(GL_TEXTURE_2D);

    glPopMatrix();

    restoreGLState();
}

void mGLWidget::saveGLState()
{
    glPushAttrib(GL_ALL_ATTRIB_BITS);
    glMatrixMode(GL_PROJECTION);
    glPushMatrix();
    glMatrixMode(GL_MODELVIEW);
    glPushMatrix();
}

void mGLWidget::restoreGLState()
{
    glMatrixMode(GL_PROJECTION);
    glPopMatrix();
    glMatrixMode(GL_MODELVIEW);
    glPopMatrix();
    glPopAttrib();
}

//** Begin(Move) into vesUIElement
QSize mGLWidget::calculateTextureSize( int width, int height )
{
    QSize s( 2, 2 );  // Initialize at the usable minimum
    s.setWidth( int( pow( 2, ceil( log( width ) / log( 2 ) ) ) ) );
    s.setHeight( int( pow( 2, ceil( log( height ) / log( 2 ) ) ) ) );
    return s;
}

void mGLWidget::keyPressEvent ( QKeyEvent *event )
{
    //m_scene->setFocus( Qt::ActiveWindowFocusReason );
    //bool foc = m_scene->hasFocus();

//    if (event->key() == Qt::Key_Tab)
//    {
//        QWidget *old = m_dialog->focusWidget();
//        if (old != NULL)
//        {
//            QFocusEvent *foe = new QFocusEvent( QEvent::FocusOut, Qt::TabFocusReason );
//            QApplication::sendEvent( old, foe );
//            QApplication::sendEvent( old->style(), foe);
//        }
//        QWidget *next = m_dialog->nextInFocusChain();
//        QKeyEvent *e = new QKeyEvent( event->type(), event->key(),
//                                      event->modifiers(), event->text(),
//                                      event->isAutoRepeat(), event->count() );
//        QApplication::sendEvent( m_dialog, e );
//        QFocusEvent *fie = new QFocusEvent( QEvent::FocusIn, Qt::TabFocusReason );
//        QApplication::sendEvent( next, fie );
//        QApplication::sendEvent( next->style(), fie);
//    }
//    else if (event->key() == Qt::Key_Backtab)
//    {
//        QWidget *old = m_dialog->focusWidget();
//        if (old != NULL)
//        {
//            QFocusEvent *foe = new QFocusEvent( QEvent::FocusOut, Qt::TabFocusReason );
//            QApplication::sendEvent( old, foe );
//        }
//        QWidget *next = m_dialog->previousInFocusChain();
//        QKeyEvent *e = new QKeyEvent( event->type(), event->key(),
//                                      event->modifiers(), event->text(),
//                                      event->isAutoRepeat(), event->count() );
//        QApplication::sendEvent( m_dialog, e );
//
//        QFocusEvent *fie = new QFocusEvent( QEvent::FocusIn, Qt::TabFocusReason );
//        QApplication::sendEvent( next, fie );
//    }
//    else
    {
        sendKeyEvent( event );
    }
}

void mGLWidget::keyReleaseEvent ( QKeyEvent *event )
{
    sendKeyEvent( event );
}

void mGLWidget::sendKeyEvent( QKeyEvent *event )
{
    qt_sendSpontaneousEvent(m_view, event);
}

void mGLWidget::mouseMoveEvent( QMouseEvent * event )
{
    printMouseCoordinates( event->x(), event->y() );
    sendMouseEvent( event );
}

void mGLWidget::mousePressEvent( QMouseEvent *event )
{
    sendMouseEvent( event );
}

void mGLWidget::mouseReleaseEvent( QMouseEvent *event )
{
    sendMouseEvent( event );
}

void mGLWidget::sendMouseEvent( QMouseEvent *event )
{

//    QMouseEvent *e = new QMouseEvent( event->type(),
//                                      event->pos(),
//                                      event->button(), event->buttons(),
//                                      event->modifiers() );
//    QApplication::sendEvent( m_view, event );

    //QPoint pos( event->x(), event->y() );
    QPoint globalPos = m_view->viewport()->mapToGlobal(event->pos());
    QWidget *vw = m_view->childAt(event->x(),event->y());
    if ( vw == NULL ) return;
//    QMouseEvent e( event->type(),
//                                      vw->mapFrom( m_view, pos ),
//                                      event->button(), event->buttons(),
//                                      event->modifiers() );
    //QApplication::sendEvent( vw, &e );
    QMouseEvent e( event->type(), event->pos(), globalPos, event->button(),
                   event->buttons(), event->modifiers());
    qt_sendSpontaneousEvent( m_view->viewport(), &e );

    // Update focus if buttonpress
//    if ( event->type() == QEvent::MouseButtonPress )
//    {
//        QWidget *dw = m_dialog->childAt(event->x(),event->y());
//        if( ( dw != NULL ) && ( dw != m_dialog->focusWidget() ) )
//        {
//            if( dw->focusPolicy() != Qt::NoFocus )
//            {
//                QWidget *old = m_dialog->focusWidget();
//                if (old != NULL)
//                {
//                    QFocusEvent *foe = new QFocusEvent( QEvent::FocusOut, Qt::MouseFocusReason );
//                    QApplication::sendEvent( old, foe );
//                    QApplication::sendEvent( old->style(), foe );
//                }
//                dw->setFocus();
//                QFocusEvent fie( QEvent::FocusIn, Qt::MouseFocusReason );
//                QApplication::sendEvent( dw, &fie );
//                //QApplication::sendEvent( dw->style(), &fie );
//            }
//        }
//    }

}
//** End(Move)

void mGLWidget::printMouseCoordinates( int x, int y )
{
    m_dialog->findChild<QLabel *>( "label_x" )->setNum( x );
    m_dialog->findChild<QLabel *>( "label_y" )->setNum( y );
}
