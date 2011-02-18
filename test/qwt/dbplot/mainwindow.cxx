
// --- QT Includes --- //
#include <QtGui/qlabel.h>
#include <QtGui/qlayout.h>

// --- QWT Includes --- //
#include <qwt_scale_engine.h>

// --- DB Plot Includes --- //
#include "mainwindow.h"
#include "plot.h"
#include "WheelBox.h"

////////////////////////////////////////////////////////////////////////////////
MainWindow::MainWindow( QWidget* parent )
    :
    QWidget( parent ),
    m_plot( NULL ),
    m_timerWheel( NULL ),
    m_intervalWheel( NULL )
{
    //seconds
    double const intervalLength = 10.0;

    m_plot = new Plot( this );
    m_plot->setIntervalLength( intervalLength );

    m_intervalWheel = new WheelBox( "Displayed [s]", 1.0, 100.0, 1.0, this );
    m_intervalWheel->setValue( intervalLength );

    m_timerWheel = new WheelBox( "Sample Interval [ms]", 0.1, 50.0, 0.1, this );
    m_timerWheel->setValue( 10.0 );

    QHBoxLayout* vLayout1 = new QHBoxLayout();
    vLayout1->addWidget( m_intervalWheel );
    vLayout1->addWidget( m_timerWheel );

    QVBoxLayout* layout = new QVBoxLayout( this );
    layout->addWidget( m_plot, 10 );
    layout->addLayout( vLayout1 );

    connect(
        m_timerWheel,
        SIGNAL( valueChanged( double ) ),
        SIGNAL( signalIntervalChanged( double ) ) );
    connect(
        m_intervalWheel,
        SIGNAL( valueChanged( double ) ),
        m_plot,
        SLOT( setIntervalLength( double ) ) );
}
////////////////////////////////////////////////////////////////////////////////
void MainWindow::start()
{
    m_plot->start();
}
////////////////////////////////////////////////////////////////////////////////
double MainWindow::signalInterval() const
{
    return m_timerWheel->value();
}
////////////////////////////////////////////////////////////////////////////////
