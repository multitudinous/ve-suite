/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2004 by Iowa State University
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
 * File:          $RCSfile: cfdSequence.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#ifndef _VRAC_CFD_SEQUENCE_H_
#define _VRAC_CFD_SEQUENCE_H_

enum cfdPlayMode{
   CFDSEQ_STOP = 0,
   CFDSEQ_START,
   CFDSEQ_PAUSE,
   CFDSEQ_RESUME,
   CFDSEQ_PLAYING,   // not in pfSequence
   CFDSEQ_STEP       // not in pfSequence
};

enum cfdLoopMode{
   CFDSEQ_CYCLE = 0,
   CFDSEQ_SWING
};

class pfNode;
class pfType;

#ifndef _USE_CFD_SEQUENCE
class pfSequence;

class cfdSequence
{
public:
   cfdSequence();
   ~cfdSequence();
   pfNode* getParent( int index );

#else // _USE_CFD_SEQUENCE

class pfSwitch;
class pfTraverser;
#include <Performer/pf/pfGroup.h>

class cfdSequence : public pfGroup
{
public:
   cfdSequence();
   ~cfdSequence();

   //to make this a performer class
   static void init(void);
#endif //_USE_CFD_SEQUENCE

   static pfType* getClassType( void );

   //get the sequence node
   pfNode * getNode();

   // set/get the duration (in seconds) of any particular frame
   void setTime( double time );
   double getTime();

   // set/get the duration (in seconds) of the entire sequence
   void setDuration( double duration );
   double getDuration();

   // set/get the interval
   void setInterval( int loopmode, int begin, int end );
   int getBegin();
   int getEnd();

   // set/get the loop mode of the sequence
   void setLoopMode( int lMode );
   int getLoopMode();

   // set/get the stop/start/pause/resume 
   void setPlayMode( int pMode );
   int getPlayMode();

   // set the current frame
   void setCurrentFrame( int frameIndex );

   // step the sequence one frame in a particular direction
   void stepSequence();

   //get the direction of the sequence
   int getDirection();

   //change the direction of the frame viewing
   //if it is currently foward(low->high index)
   //change to reverse(high->low index) and vice versa
   void changeSequenceDirection();

   //get the current frame index
   int getFrame();

   //get the next frame index
   int getNextFrame();

   //get number of children
   //virtual 
   int getNumChildren();

   //add a child node
   //virtual 
   void addChild( pfNode* child );

   //get the index of a child
   //virtual 
   int searchChild( pfNode* child );

   //get the specified child node  
   //virtual 
   pfNode* getChild( int index );

   //remove child 
   //virtual 
   int removeChild( pfNode* child );
   
#ifdef _USE_CFD_SEQUENCE
    //the node pre-traverser callback
   friend int switchFrame(pfTraverser* trav, void* userData);
#endif

protected:

#ifdef _USE_CFD_SEQUENCE
   pfSwitch* _switch;
#else
   pfSequence * _pfSequence;
#endif

   int _appFrame;
   int _lMode;
   int _pMode;
   int _step;
   int _changedDirection;

   double _deltaT;
   double _duration;

   int _begin;
   int _end;
   
   int _currentFrame;
   int _dir;   //forward(1)/backward(-1)

   static pfType* _classType;
};

#endif //_VRAC_CFD_SEQUENCE_H_
