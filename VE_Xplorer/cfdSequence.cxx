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
 * File:          $RCSfile: cfdSequence.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "cfdSequence.h"
#include <iostream>
#include <cmath>

#include <Performer/pf/pfNode.h>
#include <Performer/pf/pfSwitch.h>
#include <Performer/pf/pfTraverser.h>

using namespace std;
//Performer statics
//this code is for performer compliance
//it allows performer to determine the class type
//
pfType* cfdSequence::_classType = 0;

//initialize our class w/ performer at run time
void cfdSequence::init(void)
{
   if(_classType == 0)
   {
      //initialize the parent
      pfGroup::init();
      //create the new class type
      _classType = new pfType(pfGroup::getClassType(),"cfdSequence");
   }
}
////////////////////////////////////////
//our class implementation
//////////////////////////
//Constructors          //
//////////////////////////
cfdSequence::cfdSequence()
:pfGroup()
{
   _switch = 0;
   _duration = 0;
   _begin = 0;
   _end = 1;
   _dir = 1;
   _currentFrame = 0;
   _lMode = 0;
   _pMode = 1;
   _appFrame = 0;
   _step = -10000;

   setTravFuncs(PFTRAV_APP,switchFrame,0);
   setTravData(PFTRAV_APP,this);

   //performer stuff
   init();
   setType(_classType);
}
///////////////////////////
//Destructor             //
///////////////////////////
cfdSequence::~cfdSequence()
{
}
/////////////////////////////////
int cfdSequence::getNumChildren()
{
   if(_switch){
      return _switch->getNumChildren();
   }
   return -1;
}
///////////////////////////////////////////
int cfdSequence::removeChild(pfNode* child)
{
   if(_switch){
      return _switch->removeChild(child);
   }
   return -1;
}
////////////////////////////////////////////
int cfdSequence::searchChild(pfNode* child)
{
   if(_switch){
      return _switch->searchChild(child);   
   }
   return -1;
}
////////////////////////////////////////
pfNode* cfdSequence::getChild(int index)
{
   if(_switch){
      //cout<<"getting child: "<<index<<endl;
      return _switch->getChild(index);
   }
   return 0;
}
/////////////////////////////////////////////////////////////////
//the pre node traverser callback to decide which frame to show//
/////////////////////////////////////////////////////////////////
int switchFrame(pfTraverser* trav, void* userData)
{
   //cout<<"Traversing cfdSequence node."<<endl;
   //the speed of the sequence(fps)
   double seqRate = 0;
   
   //the number of frames
   int nChildren = 0;
 
   //the sequence interval params
   int end = 0;
   int begin = 0;

   //the loop mode
   int lMode = 0;
 
   //the play mode(stop,start,pause,resume,playing)
   int pMode = 0;

   //length of sequence (secs)
   double duration = 0;

   //the frame rate the app is running at
   double appFrameRate = pfGetFrameRate();

   //the ratio of application frame rate 
   //to the desired sequence rate
   double frameRateRatio = 0;
   
   //the number of frames to increment 
   int deltaFrame = 1;

   //the sequence node w/ all the desired state info
   cfdSequence* sequence = (cfdSequence*)userData;

   nChildren = sequence->_switch->getNumChildren();

   duration = sequence->_duration;
   end = sequence->_end;
   begin = sequence->_begin;
   lMode = sequence->_lMode;
   pMode = sequence->_pMode;

   //cout<<"Number of frames: "<<nChildren<<endl;
   //cout<<"Beginning :"<<begin<<endl;
   //cout<<"End :"<<end<<endl;
   //cout<<"Loop Mode: "<<lMode<<endl;
   //cout<<"Play Mode: "<<pMode<<endl;   
   
   //make sure we have a valid interval 
   if(sequence->_dir ==1){
      if(end >  nChildren - 1){
         end = nChildren -1;
      }

      if(begin < 0){
        begin = 0;
      }
   }else if(sequence->_dir == -1){
      if(begin >  nChildren - 1){
         begin = nChildren -1;
      }

      if(end < 0){
        end = 0;
      }
   }

   //check if we need to update the sequence 
   //node 

   //the desired rate of the sequence
   seqRate = fabs((double)(begin - end))/duration;

   //cout<<"Sequence rate: "<<seqRate<<endl;
   

   //if we haven't passed enough frames in 
   //the app we don't need to update yet

   //the app is processing faster than sequence
   if(appFrameRate >= seqRate && pMode != CFDSEQ_START){
      //cout<<"Frame Rate: "<<appFrameRate<<endl;
      //calculate frame rate ratio
      frameRateRatio = appFrameRate/seqRate;

      //since we're running slower than the
      //application, we don't need to update yet
      if(sequence->_appFrame < frameRateRatio){
         //continue in the current state 
         //update the frames the app has processed
         sequence->_appFrame++;
         //cout<<"App frames: "<<sequence->_appFrame<<endl;
         return PFTRAV_CONT;
      }
       //cout<<"Switching frames!"<<endl;
   }else if(appFrameRate < seqRate){
      //cout<<"Frame rate is slow."<<endl;
      //app is running slower than the sequence
      //need to skip some frames in the
      //sequence 
      deltaFrame = seqRate/appFrameRate;
   }

   //now check the states and do the updates

   //we are in stop or pause
   if ( pMode == CFDSEQ_STOP || pMode == CFDSEQ_PAUSE ){
      //don't change the scene graph
#ifdef _DEBUG
      cout<<"Stopped or paused!"<<endl;
#endif
      return PFTRAV_CONT;
   }


   //if we've made it here, we are either resuming
   //or starting so check the loop mode and direction
   //to update the graph accordingly

   //we're restarting so reset current frame 
   //in the sequence to the beginning
   if ( pMode == CFDSEQ_START ){
#ifdef _DEBUG
      cout<<"Starting sequence."<<endl;
#endif
      sequence->_currentFrame = begin;
      sequence->_switch->setVal(sequence->_currentFrame);

      //notify we are that we have started
      sequence->setPlayMode( CFDSEQ_PLAYING );
      return PFTRAV_CONT;
   }

   //depending on cycle type, decide the frame
   //to display
   if(sequence->_switch){
      //get the next frame to display
      sequence->_currentFrame = sequence->getNextFrame();
      
      //set the displayed frame on the switch
      sequence->_switch->setVal(sequence->_currentFrame);

      //handle swing loop mode by changing directions
      //when we get to the beginning or end of the sequence
      if(lMode == CFDSEQ_SWING){
         //(swing) so go back and forth through frames 
         if(sequence->_currentFrame == end|| 
            sequence->_currentFrame == begin){
	         //switch the direction of the sequence
            sequence->_dir *= -1;
         }
      }
      //reset the appFrame counter for synchronization
      sequence->_appFrame = 0;

      //if we are stepping, pause the sequence
      if(sequence->_step == CFDSEQ_STEP){
         sequence->_pMode = CFDSEQ_PAUSE;
         sequence->_step = 0;
      }
   }
   

   return PFTRAV_CONT;
} 
/////////////////////////////////////////////////////////
void cfdSequence::setInterval(int mode, int beg, int end)
{
   //cout<<"Setting interval."<<endl;
   //loop mode cycle or swing
   _lMode = mode; 

   //beginning and ending of sequence interval
   _begin = beg;
   _end = end;

   //check which direction we are going
   if(_begin > _end)_dir = -1;
   else if(_end > _begin) _dir = 1;
}
////////////////////////////////////////////////
//change the direction of the frame sequencing//
////////////////////////////////////////////////
void cfdSequence::changeSequenceDirection()
{
   //update the direction
   setInterval(_lMode,_end,_begin);
}
////////////////////////////////////////////
//set the current frame for cluster       // 
//implementation                          // 
////////////////////////////////////////////
void cfdSequence::setCurrentFrame(int index)
{
   //no need to update yet
   if(_currentFrame == index) return;

   //check direction and see if index
   //is valid
   if(_dir == 1){
      //low to high
      if(index >= _begin && index < _end){
         _currentFrame = index;
	      _switch->setVal(_currentFrame);
      }else{
         //invalid index
         cout<<"Error: cfdSequence!"<<endl;
         cout<<"Frame index: "<<index
             <<" is outside of range of valid frames!"<<endl;
         return;
      }
   }else if(_dir == -1){
      //high to low
      if(index >= _end && index < _begin){
         _currentFrame = index;
	 _switch->setVal(_currentFrame);
      }else{
         //invalid index
         cout<<"Error: cfdSequence!"<<endl;
         cout<<"Frame index: "<<index
             <<" is outside of range of valid frames!"<<endl;
         return;
      }
   }
   return; 
}
/////////////////////////////////////////
//add a child                          //
/////////////////////////////////////////
void cfdSequence::addChild(pfNode* child)
{
   //cout<<"Adding frame to sequence."<<endl;
   //init the switch node
   if(!_switch){
      _switch = new pfSwitch();
      _switch->setVal(PFSWITCH_OFF);
      
      //add the switch node to the tree
      pfGroup::addChild(_switch);
   }
  
   _switch->addChild(child);
}
///////////////////////////
//adjusted this to handle//
//the last frame of the  //
//sequence in cycle mode //
///////////////////////////
int cfdSequence::getFrame()
{
   return _currentFrame;
}
/////////////////////////////////////////
//get the frame that is currently being// 
//displayed on the tree                //
/////////////////////////////////////////
int cfdSequence::getNextFrame()
{ 
   if(_currentFrame == _end && _lMode == CFDSEQ_CYCLE){
      return _begin;
   }else{
      //update the frame in the sequence
      return _currentFrame + _dir;
   }
}

