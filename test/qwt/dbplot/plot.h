
#ifndef PLOT_H
#define PLOT_H

// --- QWT Includes --- //
#include <qwt_plot.h>
#include <qwt_interval.h>
#include <qwt_system_clock.h>

class QwtPlotCurve;
class QwtPlotMarker;
class QwtPlotDirectPainter;

class Plot: public QwtPlot
{
    ///
    Q_OBJECT

public:
    ///
    Plot( QWidget* parent = NULL );

    ///
    virtual ~Plot();

    ///
    void start();

    ///
    virtual void replot();

public Q_SLOTS:
    ///
    void setIntervalLength( double interval );

protected:
    ///
    virtual void resizeEvent( QResizeEvent* resizeEvent );

    ///
    virtual void timerEvent( QTimerEvent* timerEvent );

private Q_SLOTS:
    ///
    void showCurve( QwtPlotItem*, bool on );

private:
    ///
    void updateCurves();

    ///
    void incrementInterval();

    ///
    int m_timerId;

    ///
    QwtInterval m_interval;

    ///
    QwtPlotMarker* m_marker;

    ///
    std::vector< QwtPlotCurve* > m_curves;

    ///
    QwtSystemClock m_clock;

    ///
    QwtPlotDirectPainter* m_directPainter;

};

#endif //PLOT_H
