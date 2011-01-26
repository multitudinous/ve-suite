
// --- QT Includes --- //
#include <QtGui/qevent.h>


// --- QT Includes --- //
#include <QtCore/qdatetime.h>

// --- QWT Includes --- //
#include <qwt_plot_grid.h>
#include <qwt_plot_layout.h>
#include <qwt_plot_canvas.h>
#include <qwt_plot_marker.h>
#include <qwt_plot_curve.h>
#include <qwt_plot_directpainter.h>
#include <qwt_curve_fitter.h>
#include <qwt_painter.h>
#include <qwt_scale_draw.h>
#include <qwt_scale_widget.h>
#include <qwt_legend.h>
#include <qwt_legend_item.h>

// --- DB Plot Includes --- //
#include "plot.h"
#include "SensorData.h"

// --- STL Includes --- //
#include <iostream>

////////////////////////////////////////////////////////////////////////////////
Plot::Plot( QWidget* parent )
    :
    QwtPlot( parent ),
    m_timerId( -1 ),
    m_paintedPoints( 0 ),
    //Seconds
    m_interval( 0.0, 10.0 ),
    m_marker( new QwtPlotMarker() ),
    m_curve( new QwtPlotCurve() ),
    m_clock(),
    m_directPainter( new QwtPlotDirectPainter() )
{
    setAutoReplot( false );
    //setCanvasBackground( Qt::black );
    setTitle( "Sensor Data" );
    plotLayout()->setAlignCanvasToScales( true );

    QwtLegend* legend = new QwtLegend();
    legend->setItemMode( QwtLegend::CheckableItem );
    insertLegend( legend, QwtPlot::RightLegend );

    setAxisTitle( QwtPlot::xBottom, "Time [h:m:s]" );
    setAxisScale(
        QwtPlot::xBottom, m_interval.minValue(), m_interval.maxValue() );
    setAxisTitle( QwtPlot::yLeft, "Values [unit]" );
    setAxisScale( QwtPlot::yLeft, -100.0, 100.0 );

    //We don't need the cache here
    canvas()->setPaintAttribute( QwtPlotCanvas::PaintCached, false );
    //canvas()->setPaintAttribute( QwtPlotCanvas::PaintPacked, false );

#if defined(Q_WS_X11)
    //Even if not recommended by TrollTech, Qt::WA_PaintOutsidePaintEvent
    //works on X11. This has a nice effect on the performance.
    canvas()->setAttribute( Qt::WA_PaintOutsidePaintEvent, true );
    canvas()->setAttribute( Qt::WA_PaintOnScreen, true );
#endif

    QwtPlotGrid* grid = new QwtPlotGrid();
    grid->setPen( QPen( Qt::gray, 0.0, Qt::DotLine ) );
    grid->enableX( true );
    grid->enableXMin( true );
    grid->enableY( true );
    grid->enableYMin( false );
    grid->attach( this );

    //Currently not used
    m_marker->setValue(  QPointF( 0.0, 0.0 ) );
    m_marker->setLineStyle( QwtPlotMarker::VLine );
    m_marker->setLinePen( QPen( Qt::gray, 1, Qt::SolidLine ) );
    //m_marker->attach( this );

    m_curve->setStyle( QwtPlotCurve::Lines );
    m_curve->setPen( QPen( Qt::green ) );
    m_curve->setRenderHint( QwtPlotItem::RenderAntialiased, true );
    m_curve->setPaintAttribute( QwtPlotCurve::ClipPolygons, false );
    m_curve->setData( new SensorData() );
    m_curve->attach( this );
}
////////////////////////////////////////////////////////////////////////////////
Plot::~Plot()
{
    delete m_directPainter;
}
////////////////////////////////////////////////////////////////////////////////
void Plot::start()
{
    //Start the sensor data sampling thread first
    SensorData* sensorData = static_cast< SensorData* >( m_curve->data() );
    SamplingThread& samplingThread = sensorData->GetSamplingThread();
    samplingThread.start();

    //Need QwtSystemClock to handle sampling measurements
    //It is more accurate than QTime
    m_clock.start();
    //This is how often the timerEvent gets executed, basically a draw refresh rate
    //So here we tell m_directPainter to draw new points every 20 milliseconds
    m_timerId = startTimer( 20 );
}
////////////////////////////////////////////////////////////////////////////////
void Plot::replot()
{
    SensorData* sensorData = static_cast< SensorData* >( m_curve->data() );
    sensorData->lock();

    QwtPlot::replot();
    m_paintedPoints = sensorData->size();

    sensorData->unlock();
}
////////////////////////////////////////////////////////////////////////////////
void Plot::setIntervalLength( double interval )
{
    if( interval > 0.0 && interval != m_interval.width() )
    {
        m_interval.setMaxValue( m_interval.minValue() + interval );
        setAxisScale(
            QwtPlot::xBottom, m_interval.minValue(), m_interval.maxValue() );

        replot();
    }
}
////////////////////////////////////////////////////////////////////////////////
void Plot::updateCurve()
{
    SensorData* sensorData = static_cast< SensorData* >( m_curve->data() );
    sensorData->lock();

    int const numPoints = sensorData->size();
    if( numPoints > m_paintedPoints )
    {
        m_directPainter->drawSeries(
            m_curve, m_paintedPoints - 1, numPoints - 1 );
        //m_marker->setXValue( sensorData->value( numPoints - 1 ).x() );
        m_paintedPoints = numPoints;
    }

    sensorData->unlock();
}
////////////////////////////////////////////////////////////////////////////////
void Plot::incrementInterval()
{
    //Set interval min/max values to next screen
    m_interval = QwtInterval(
        m_interval.maxValue(), m_interval.maxValue() + m_interval.width() );

    SensorData* sensorData = static_cast< SensorData* >( m_curve->data() );
    //Clear values from previous interval
    sensorData->clearStaleValues( m_interval.minValue() );

    //To avoid, that the grid is jumping, we disable
    //the autocalculation of the ticks and shift them
    //manually instead.
    QwtScaleDiv scaleDiv = *axisScaleDiv( QwtPlot::xBottom );
    scaleDiv.setInterval( m_interval );
    for( int i = 0; i < QwtScaleDiv::NTickTypes; ++i )
    {
        QList< double > ticks = scaleDiv.ticks( i );
        for( int j = 0; j < ticks.size(); ++j )
        {
            ticks[ j ] += m_interval.width();
        }
        scaleDiv.setTicks( i, ticks );
    }

    setAxisScaleDiv( QwtPlot::xBottom, scaleDiv );

    m_paintedPoints = 0;
    replot();
}
////////////////////////////////////////////////////////////////////////////////
void Plot::timerEvent( QTimerEvent* event )
{
    if( event->timerId() == m_timerId )
    {
        updateCurve();

        //Use the more accurate QwtSystemClock to get elapsed time
        double const elapsed = m_clock.elapsed() / 1000.0;
        if( elapsed > m_interval.maxValue() )
        {
            incrementInterval();
        }

        return;
    }

    QwtPlot::timerEvent( event );
}
////////////////////////////////////////////////////////////////////////////////
void Plot::resizeEvent( QResizeEvent* event )
{
    m_directPainter->reset();
    QwtPlot::resizeEvent( event );

    const QColor color( 77, 77, 77 );
    const QRect cr = canvas()->contentsRect();
    QLinearGradient gradient( cr.topLeft(), cr.topRight() );
    gradient.setColorAt( 0.0, color.light( 130 ) );
    gradient.setColorAt( 0.2, color.dark( 110 ) );
    gradient.setColorAt( 0.7, color );
    gradient.setColorAt( 1.0, color.dark( 150 ) );

    QPalette pal = canvas()->palette();
    pal.setBrush( QPalette::Window, QBrush( gradient ) );
    canvas()->setPalette( pal );
}
////////////////////////////////////////////////////////////////////////////////
