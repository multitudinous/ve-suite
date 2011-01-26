
// --- QT Includes --- //
#include <QtGui/qapplication.h>

// --- DB Plot Includes --- //
#include "mainwindow.h"
#include "samplingthread.h"

////////////////////////////////////////////////////////////////////////////////
int main( int argc, char** argv )
{
    QApplication app( argc, argv );

    MainWindow window;
    window.resize( 800, 600 );

    window.show();
    window.start();

    return app.exec();
}
////////////////////////////////////////////////////////////////////////////////
