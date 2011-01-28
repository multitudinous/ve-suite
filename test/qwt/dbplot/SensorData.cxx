
// --- DB Plot Includes --- //
#include "SensorData.h"

// --- STL Includes --- //
#include <iostream>

////////////////////////////////////////////////////////////////////////////////
SensorData::SensorData()
    :
    m_paintedPoints( 0 ),
    m_samplingThread( this ),
    m_lock(),
    m_values(),
    m_mutex(),
    m_pendingValues()
{
    //This is how often we will push back points to SensorData in milliseconds
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

    bool const isLocked = m_lock.tryLockForWrite();
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
        double const time = (*itr).x();
        if( time < minimum )
        {
            itr = m_values.erase( itr );
        }
        else
        {
            break;
        }
    }

    m_lock.unlock();
}
////////////////////////////////////////////////////////////////////////////////
unsigned int const& SensorData::GetNumPaintedPoints() const
{
    return m_paintedPoints;
}
////////////////////////////////////////////////////////////////////////////////
unsigned int SensorData::GetNumPoints()
{
    m_lock.lockForRead();
    unsigned int const numPoints = size();
    m_lock.unlock();

    return numPoints;
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
void SensorData::SetNumPaintedPoints( unsigned int const& numPoints )
{
    m_paintedPoints = numPoints;
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
