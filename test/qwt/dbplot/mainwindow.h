
#ifndef MAIN_WINDOW_H
#define MAIN_WINDOW_H

// --- QT Includes --- //
#include <QtGui/qwidget.h>

// --- DB Plot Includes --- //
class Plot;

class MainWindow : public QWidget
{
    ///
    Q_OBJECT

public:
    ///
    MainWindow( QWidget* parent = NULL );

    ///
    void start();

    ///
    //double signalInterval() const;

Q_SIGNALS:
    ///
    //void signalIntervalChanged( double );

private:
    ///
    Plot* d_plot;

};

#endif //MAIN_WINDOW_H
