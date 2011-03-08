
// --- VES Includes --- //
#include <ves/xplorer/eventmanager/EventManager.h>

// --- QT Includes --- //
#include <QtGui/qapplication.h>

// --- DB Plot Includes --- //
#include "mainwindow.h"
#include "CANMessageParser.h"

////////////////////////////////////////////////////////////////////////////////
int main( int argc, char** argv )
{
    QApplication app( argc, argv );

    MainWindow window;
    window.resize( 800, 600 );

    ves::xplorer::eventmanager::EventManager::instance();

    CANMessageParser canThread;
    canThread.setInterval( window.signalInterval() );

    window.connect(
        &window,
        SIGNAL( signalIntervalChanged( double ) ),
        &canThread,
        SLOT( setInterval( double ) ) );

    window.show();

    canThread.start();
    window.start();

    bool ok = app.exec();

    ves::xplorer::eventmanager::EventManager::instance()->Shutdown();

    canThread.stop();
    canThread.wait( 1000 );

    return ok;
}
////////////////////////////////////////////////////////////////////////////////
