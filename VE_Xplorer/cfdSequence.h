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
   CFDSEQ_PLAYING 
};

enum cfdLoopMode{
CFDSEQ_CYCLE = 0,
CFDSEQ_SWING 
};

class cfdSequence : public pfGroup{
public:
   cfdSequence();
   ~cfdSequence();

   //set the duration of the sequence
   void setDuration(double duration){_duration = duration;}

   //set the interval
   void setInterval(int mode, int beg, int end);

   //set the loop of the sequence
   void setLoopMode(int lMode){_lMode = lMode;}

   //stop/start/pause/resume 
   void setPlayMode(int pMode){_pMode = pMode;}

   //set the current frame
   void setCurrentFrame(int frameIndex);

   //get the current frame index
   int getFrame(){return _currentFrame;}

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
   
   //get the duration of the sequence
   double duration(){return _duration;}
   
   
  friend  int switchFrame(pfTraverser* trav, void* userData);
protected:
   //the node pre-traverser callback

   pfSwitch* _switch;

   int _appFrame;
   int _lMode;
   int _pMode;

   double _deltaT;
   double _duration;

   int _begin;
   int _end;
   
   int _currentFrame;
   //forward(1)/backward(-1)
   int _dir;
   
};

#endif //_VRAC_CFD_SEQUENCE_H_
