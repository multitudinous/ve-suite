#ifndef MGLWIDGET_H
#define MGLWIDGET_H

#include <QtOpenGL/QtOpenGL>
#include <QtOpenGL/QGLWidget>
#include "mydialog.h"

class mGLWidget : public QGLWidget
{
public:
    mGLWidget(QWidget *parent);
    virtual ~mGLWidget();

   void keyPressEvent ( QKeyEvent * event );
   void keyReleaseEvent ( QKeyEvent * event );
   void mouseMoveEvent( QMouseEvent * event );
   void mousePressEvent( QMouseEvent *event );
   void mouseReleaseEvent( QMouseEvent *event );

protected:
    void paintGL();
    void initializeGL();
    void saveGLState();
    void restoreGLState();

    void sendKeyEvent( QKeyEvent *event );
    void sendMouseEvent( QMouseEvent *event );
    void printMouseCoordinates( int x, int y );
    QSize calculateTextureSize( int width, int height );

private:
    QDialog *m_dialog;
    QGLFramebufferObject *m_frameBuffer;
    QImage *m_image;
    GLuint m_glList;
    QTimer *m_timer;
    QGraphicsScene *m_scene;
    QGraphicsProxyWidget *m_proxy;
    QGraphicsView *m_view;
};

#endif // MGLWIDGET_H
