#include <QtGui/QApplication>
#include "mglwidget.h"

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);

    //myDialog w( 0 );
    //w.show();

    mGLWidget glView( 0 );
    glView.resize( 640, 512 );
    glView.show();



    return a.exec();
}
