/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/

#ifndef VE_VOICE_CIRCULAR_QUEUE_H_
#define VE_VOICE_CIRCULAR_QUEUE_H_

#include <cstdlib>
/**
 * A CircularQueue data structure implementation intended for use with a
 * single producer and a single consumer thread.  The idea was take from
 * the article "Thread-Safe Circular Queue" by Cladio Taglienti in the
 * June 2004 issue of C/C++ Users Journal.
 */
template <class DATA>
class CircularQueue
{
public:

    /// Trait that represents the type of data this queue contains.
    typedef DATA DataType;

    /**
     * Default Ctor
     *
     * @param   capacity     the maximum number of items the queue can store.
     */
    CircularQueue( const size_t capacity = 1023 )
        : mData( new DataType[capacity + 1] ), mCapacity( capacity + 1 ),
          mMaxItemCnt( capacity ), mSize( 0 ), mInIdx( 0 ), mOutIdx( 0 )
    {}

    /**
     * Destructor
     */
    ~CircularQueue()
    {
        delete[] mData;
        mData = 0;
    }

    /**
     * Adds an element to this queue; returns true if successful, false
     * if the queue is full.
     *
     * @param   data     the element to add.
     *
     * @return     true if successful, false if the queue is full.
     */
    bool add( const DataType& data )
    {
        if( !isFull() )
        {
            mData[mInIdx] = data;
            mInIdx = ( mInIdx + 1 ) % mCapacity;
            ++mSize;
            return true;
        }
        return false;
    }

    /**
     * Removes an element from this queue; returns false if the queue is
     * empty.
     *
     * @param   data     a reference that will be set to the element that is
     *                   removed.
     *
     * @return     true if succesfful, false if the queue is empty.
     */
    bool remove( DataType& data )
    {
        if( !isEmpty() )
        {
            data = mData[mOutIdx];
            mOutIdx = ( mOutIdx + 1 ) % mCapacity;
            --mSize;
            return true;
        }
        return false;
    }

    /**
     * Checks to see if the queue is full.
     *
     * @return     true if the queue is full, false otherwise.
     */
    bool isFull() const
    {
        return ( ( mInIdx + 1 ) % mCapacity == mOutIdx );
    }

    /**
     * Checks to see if the queue is empty.
     *
     * @return     true if the queue is empty, false otherwise.
     */
    bool isEmpty() const
    {
        return mInIdx == mOutIdx;
    }

    /**
     * Flushes the queue of all elements; after this operation, the queue will
     * be empty.
     */
    void flush()
    {
        mSize = 0;
        mInIdx = 0;
        mOutIdx = 0;
    }

    /**
     * Returns the maximum number of items this queue can store.
     *
     * @return     the maximum number of items this queue can store.
     */
    size_t capacity() const
    {
        return mMaxItemCnt;
    }

    /**
     * Returns the current number of elements in the queue
     *
     * @return     the current number of elements in the queue.
     */
    size_t size() const
    {
        return mSize;
    }

private:

    /// The array that is used to represent the circular queue.
    DataType*                               mData;

    /// The size of the mData array.
    size_t                                  mCapacity;

    /// The maximum number of items the queue can store.
    size_t                                  mMaxItemCnt;

    /// The current number of elements in the queue.
    size_t                                  mSize;

    /// The location of the next added element.
    size_t                                  mInIdx;

    /// The location of the next element removed.
    size_t                                  mOutIdx;
};

#endif
// vim:ts=4:sw=4:et:tw=0
