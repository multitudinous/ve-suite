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

#include <Performer/pf/pfGroup.h>
class pfNode;
class pfSwitch;
class pfTraverser;

enum cfdPlayMode{
   CFDSEQ_STOP =  0,
   CFDSEQ_START,
   CFDSEQ_PAUSE,
   CFDSEQ_RESUME,
   CFDSEQ_PLAYING,
   CFDSEQ_STEP
};

enum cfdLoopMode{
   CFDSEQ_CYCLE = 0,
   CFDSEQ_SWING

};

class cfdSequence : public pfGroup{
public:
   cfdSequence();
   ~cfdSequence();

   //to make this a performer class
   static void init(void);
   static pfType* getClassType(void){return _classType;}

   //set the duration of the sequence
   void setDuration(double duration){_duration = duration;}

   //set the interval
   void setInterval(int mode, int beg, int end);

   //set the loop of the sequence
   void setLoopMode(int lMode){_lMode = lMode;}

   //stop/start/pause/resume 
   void setPlayMode(int pMode){
      _pMode = pMode;
   }

   //set the current frame
   void setCurrentFrame(int frameIndex);

   //step the sequence one frame in a particular direction
   void stepSequence(){_step = CFDSEQ_STEP;}

   //change the direction of the frame viewing
   //if it is currently foward(low->high index)
   //change to reverse(high->low index) and vice
   //versa
   void changeSequenceDirection();

   //get the current frame index
   int getFrame();//{return _currentFrame ;}

   //get the next frame index
   int getNextFrame();

   //get number of children
   virtual int getNumChildren();

   //add a child node
   virtual void addChild(pfNode* child);

   //get the index of a child
   virtual int searchChild(pfNode* child);

   //get the specified child node  
   virtual pfNode* getChild(int index);

   //remove child 
   virtual int removeChild(pfNode* child);
   
   //get the direction of the sequence
   int direction(){return _dir;}

   //get the properties of the sequence
   double duration(){return _duration;}
   int loopMode(){return _lMode;}
   int playMode(){return _pMode;}
   int end(){return _end;}
   int begin(){return _begin;}
    //the node pre-traverser callback
  friend  int switchFrame(pfTraverser* trav, void* userData);
protected:
   pfSwitch* _switch;

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
   //forward(1)/backward(-1)
   int _dir;

   static pfType* _classType;
   
};

#endif //_VRAC_CFD_SEQUENCE_H_
