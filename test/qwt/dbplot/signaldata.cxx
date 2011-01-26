
// --- DB Plot Includes --- //
#include "signaldata.h"

// --- STL Includes --- //
#include <iostream>

////////////////////////////////////////////////////////////////////////////////
SignalData::SignalData()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
SignalData::~SignalData()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
int SignalData::size() const
{
    return m_values.size();
}
////////////////////////////////////////////////////////////////////////////////
QPointF const& SignalData::value( int index ) const
{
    return m_values.at( index );
}
////////////////////////////////////////////////////////////////////////////////
void SignalData::lock()
{
    m_lock.lockForRead();
}
////////////////////////////////////////////////////////////////////////////////
void SignalData::unlock()
{
    m_lock.unlock();
}
////////////////////////////////////////////////////////////////////////////////
void SignalData::append( QPointF const& sample )
{
    m_mutex.lock();
    m_pendingValues.push_back( sample );

    const bool isLocked = m_lock.tryLockForWrite();
    if( isLocked )
    {
        m_values.insert(
            m_values.end(), m_pendingValues.begin(), m_pendingValues.end() );
        m_pendingValues.clear();

        m_lock.unlock();
    }

    m_mutex.unlock();
}
////////////////////////////////////////////////////////////////////////////////
void SignalData::clearStaleValues( double minimum )
{
    m_lock.lockForWrite();

    std::deque< QPointF >::iterator itr = m_values.begin();
    for( ; itr != m_values.end(); )
    {
        double timeVal = itr->x();
        ++itr;
        if( timeVal < minimum )
        {
            m_values.pop_front();
        }
    }

    m_lock.unlock();
}
////////////////////////////////////////////////////////////////////////////////
SignalData& SignalData::instance()
{
    static SignalData valueVector;

    return valueVector;
}
////////////////////////////////////////////////////////////////////////////////
