
// --- QT Includes --- //
#include <QtGui/qlabel.h>
#include <QtGui/qlayout.h>

// --- QWT Includes --- //
#include <qwt_scale_engine.h>

// --- DB Plot Includes --- //
#include "mainwindow.h"
#include "plot.h"

////////////////////////////////////////////////////////////////////////////////
MainWindow::MainWindow( QWidget* parent )
    :
    QWidget( parent ),
    d_plot( new Plot( this ) )
{
    QHBoxLayout* layout = new QHBoxLayout( this );
    layout->addWidget( d_plot, 5 );

    /*
    connect(
        d_timerWheel,
        SIGNAL( valueChanged( double ) ),
        SIGNAL( signalIntervalChanged( double ) ) );
    connect(
        d_intervalWheel,
        SIGNAL( valueChanged( double ) ),
        d_plot,
        SLOT( setIntervalLength( double ) ) );
    connect(
        &window,
        SIGNAL( signalIntervalChanged( double ) ),
        &samplingThread,
        SLOT( setInterval( double ) ) );
    */
}
////////////////////////////////////////////////////////////////////////////////
void MainWindow::start()
{
    d_plot->start();
}
////////////////////////////////////////////////////////////////////////////////
/*
double MainWindow::signalInterval() const
{
    return d_timerWheel->value();
}
*/
////////////////////////////////////////////////////////////////////////////////
