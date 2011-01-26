
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
    QWidget( parent )
{
    //Seconds
    const double intervalLength = 15.0;

    d_plot = new Plot( this );
    d_plot->setIntervalLength( intervalLength );

    QHBoxLayout* layout = new QHBoxLayout( this );
    layout->addWidget( d_plot, 10 );

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
