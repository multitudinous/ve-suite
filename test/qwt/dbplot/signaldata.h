
#ifndef SIGNAL_DATA_H
#define SIGNAL_DATA_H

// --- QT Includes --- //
#include <QtCore/qpoint.h>
#include <QtCore/qmutex.h>
#include <QtCore/qreadwritelock.h>
//#include <QtCore/qrect.h>

// --- STL Includes --- //
#include <vector>
#include <deque>

class SignalData
{
public:
    ///
    static SignalData& instance();

    ///
    void append( QPointF const& sample );

    ///
    void clearStaleValues( double minimum );

    ///
    int size() const;

    ///
    QPointF const& value( int index ) const;

    ///
    void lock();

    ///
    void unlock();

protected:

private:
    ///
    SignalData();

    ///
    SignalData( SignalData const& );

    ///
    SignalData &operator=( SignalData const& );

    ///
    virtual ~SignalData();

    ///
    QReadWriteLock m_lock;

    ///
    std::deque< QPointF > m_values;

    ///
    //QRectF boundingRect;

    ///Protecting pending values
    QMutex m_mutex;
    std::vector< QPointF > m_pendingValues;

};

#endif //SIGNAL_DATA_H
