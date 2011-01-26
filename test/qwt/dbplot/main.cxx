
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

    SamplingThread samplingThread;
    //This is how often we will push back points to SignalData in milliseconds
    samplingThread.setInterval( 20.0 );

    /*
    window.connect(
        &window,
        SIGNAL( signalIntervalChanged( double ) ),
        &samplingThread,
        SLOT( setInterval( double ) ) );
    */

    window.show();

    samplingThread.start();
    window.start();

    bool ok = app.exec();

    samplingThread.stop();
    samplingThread.wait( 1000 );

    return ok;
}
////////////////////////////////////////////////////////////////////////////////
