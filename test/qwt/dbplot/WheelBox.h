
#ifndef WHEELBOX_H
#define WHEELBOX_H

// --- QT Includes --- //
#include <QtGui/qwidget.h>
class QLabel;
class QLCDNumber;

// --- QWT Includes --- //
class QwtWheel;

class WheelBox : public QWidget
{
    ///
    Q_OBJECT

public:
    ///
    WheelBox(
        QString const& title,
        double min,
        double max,
        double stepSize,
        QWidget* parent = NULL );

    ///
    void setUnit( QString const& );

    ///
    QString unit() const;

    ///
    void setValue( double value );

    ///
    double value() const;

Q_SIGNALS:
    ///
    double valueChanged( double );

private:
    ///
    QLCDNumber* m_number;

    ///
    QwtWheel* m_wheel;

    ///
    QLabel* m_label;

    ///
    QString m_unit;

};

#endif //WHEELBOX_H
