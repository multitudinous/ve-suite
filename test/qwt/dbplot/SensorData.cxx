
// --- DB Plot Includes --- //
#include "SensorData.h"

// --- STL Includes --- //
#include <iostream>

////////////////////////////////////////////////////////////////////////////////
SensorData::SensorData()
    :
    m_samplingThread( this )
{
    //This is how often we will push back points to SignalData in milliseconds
    m_samplingThread.setInterval( 20.0 );
}
////////////////////////////////////////////////////////////////////////////////
SensorData::~SensorData()
{
    m_samplingThread.stop();
    m_samplingThread.wait( 1000 );
}
////////////////////////////////////////////////////////////////////////////////
void SensorData::append( QPointF const& sample )
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
QRectF SensorData::boundingRect() const
{
    return QRectF();
}
////////////////////////////////////////////////////////////////////////////////
SamplingThread& SensorData::GetSamplingThread()
{
    return m_samplingThread;
}
////////////////////////////////////////////////////////////////////////////////
void SensorData::clearStaleValues( double minimum )
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
void SensorData::lock()
{
    m_lock.lockForRead();
}
////////////////////////////////////////////////////////////////////////////////
QPointF SensorData::sample( size_t i ) const
{
    return QPointF( m_values.at( i ) );
}
////////////////////////////////////////////////////////////////////////////////
size_t SensorData::size() const
{
    return m_values.size();
}
////////////////////////////////////////////////////////////////////////////////
void SensorData::unlock()
{
    m_lock.unlock();
}
////////////////////////////////////////////////////////////////////////////////
QPointF const& SensorData::value( int index ) const
{
    return m_values.at( index );
}
////////////////////////////////////////////////////////////////////////////////
