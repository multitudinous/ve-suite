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

#include <Performer/pf/pfNode.h>
using namespace std;

//Performer static member for performer compliance
//it allows performer to determine the class type
pfType* cfdSequence::_classType = NULL;

#include <Performer/pf/pfSwitch.h>
#include <Performer/pf/pfTraverser.h>

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
:pfGroup(),cfdGroup()
{
   _switch = 0;
   _deltaT = 0.0;
   _duration = 0.0;
   _begin = 0;
   _end = 1;
   _dir = 1;
   _currentFrame = 0;
   _lMode = CFDSEQ_CYCLE;
   _pMode = CFDSEQ_START;
   _appFrame = 0;
   _step = -10000;

   setTravFuncs(PFTRAV_APP,switchFrame,0);
   setTravData(PFTRAV_APP,this);

   //performer stuff
   init();
   setType(_classType);
   SetCFDNodeType(CFD_SEQUENCE);
   _node = this;
}
///////////////////////////////////////////////////
cfdSequence::cfdSequence(const cfdSequence& cfdSeq)
:pfGroup(),cfdGroup()
{
   _switch = cfdSeq._switch;
   _deltaT = cfdSeq._deltaT;
   _duration = cfdSeq._duration;
   _begin = cfdSeq._begin;
   _end = cfdSeq._end;
   _dir = cfdSeq._dir;
   _currentFrame = cfdSeq._currentFrame;
   _lMode = cfdSeq._lMode;
   _pMode = cfdSeq._pMode;
   _appFrame = cfdSeq._appFrame;
   _step = cfdSeq._step;

   setTravFuncs(PFTRAV_APP,switchFrame,0);
   setTravData(PFTRAV_APP,this);

   //performer stuff
   init();
   setType(_classType);
   SetCFDNodeType(CFD_SEQUENCE);
   _node = this;
}
   
///////////////////////////
//Destructor             //
///////////////////////////
cfdSequence::~cfdSequence()
{
}
///////////////////////////////////////////////////////////
cfdSequence& cfdSequence::operator=(const cfdSequence& rhs)
{
   if ( this != &rhs){
      //call parents = operator
      //cfdGroup::operator =(rhs);
      pfGroup::operator =(rhs);
      cfdGroup::operator =(rhs);

      //update everything
      _switch = rhs._switch;
      _deltaT = rhs._deltaT;
      _duration = rhs._duration;
      _begin = rhs._begin;
      _end = rhs._end;
      _dir = rhs._dir;
      _currentFrame = rhs._currentFrame;
      _lMode = rhs._lMode;
      _pMode = rhs._pMode;
      _appFrame = rhs._appFrame;
      _step = rhs._step;

      setTravFuncs(PFTRAV_APP,switchFrame,0);
      setTravData(PFTRAV_APP,this);
      SetCFDNodeType(CFD_SEQUENCE);
 

   }
   return *this;
}
void cfdSequence::setDuration( double duration )
{
   _duration = duration;
   _deltaT = duration / this->getNumChildren();
}

void cfdSequence::setLoopMode( int lMode )
{
   _lMode = lMode;
}

void cfdSequence::setPlayMode(int pMode)
{
   _pMode = pMode;
}

void cfdSequence::stepSequence()
{
   _step = CFDSEQ_STEP;
}

int cfdSequence::getNumChildren()
{
   if ( _switch )
   {
      return _switch->getNumChildren();
   }
   return -1;
}

int cfdSequence::removeChild(cfdNode* child)
{
   if ( _switch )
   {
      //return _switch->removeChild(child);
   }
   return -1;
}

int cfdSequence::searchChild(cfdNode* child)
{
   if(_switch){
      //return _switch->searchChild(child);   
   }
   return -1;
}

cfdNode* cfdSequence::getChild(int index)
{
   if(_switch){
      //cout<<"getting child: "<<index<<endl;
      //return _switch->getChild(index);
   }
   return 0;
}
/////////////////////////////////////////////////////////////////
//the pre node traverser callback to decide which frame to show//
/////////////////////////////////////////////////////////////////
int switchFrame(pfTraverser* trav, void* userData)
{
   //cout<<"Traversing cfdSequence node."<<endl;
   
   //the sequence node w/ all the desired state info
   cfdSequence* sequence = (cfdSequence*)userData;
   
   //the number of frames
   int nChildren = sequence->_switch->getNumChildren();
   //cout<<"Number of frames: "<<nChildren<<endl;

   //the sequence interval params
   int begin = sequence->_begin;
   //cout<<"Beginning :"<<begin<<endl;
   int end = sequence->_end;
   //cout<<"End :"<<end<<endl;

   //the loop mode
   int lMode = sequence->_lMode;
   //cout<<"Loop Mode: "<<lMode<<endl;
 
   //the play mode(stop,start,pause,resume,playing)
   int pMode = sequence->_pMode;
   //cout<<"Play Mode: "<<pMode<<endl;   

   //length of sequence (secs)
   double duration = sequence->_duration;

   //the frame rate the app is running at
   double appFrameRate = pfGetFrameRate();

   //the ratio of application frame rate 
   //to the desired sequence rate
   double frameRateRatio = 0;
   
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

   //check if we need to update the sequence node 

   //the desired rate of the sequence (fps)
   double seqRate = fabs((double)(begin - end))/duration;
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
      //need to skip some frames in the sequence
      //deltaFrame is the number of frames to increment
      //sjk commented out following code line to avoid irix warning,
      //The variable "deltaFrame" is set but never used.
      //int deltaFrame = (int)( seqRate / appFrameRate );
   }

   //now check the states and do the updates

   //we are in stop or pause
   if ( pMode == CFDSEQ_STOP || pMode == CFDSEQ_PAUSE ){
      //don't change the scene graph
      //cout<<"Stopped or paused!"<<endl;
      return PFTRAV_CONT;
   }

   //if we've made it here, we are either resuming
   //or starting so check the loop mode and direction
   //to update the graph accordingly

   //we're restarting so reset current frame 
   //in the sequence to the beginning
   if ( pMode == CFDSEQ_START ){
      //cout<<"Starting sequence."<<endl;
      sequence->_currentFrame = begin;
      sequence->_switch->setVal(sequence->_currentFrame);

      //notify we are that we have started
      sequence->setPlayMode( CFDSEQ_PLAYING );
      return PFTRAV_CONT;
   }

   //depending on cycle type, decide the frame to display
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
void cfdSequence::setInterval( int loopmode, int begin, int end )
{
   //cout<<"Setting interval."<<endl;
   //loop mode cycle or swing
   _lMode = loopmode; 

   //beginning and ending of sequence interval
   _begin = begin;
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
////////////////////////////////////////////////////
//set the current frame for cluster implementation//
////////////////////////////////////////////////////
void cfdSequence::setCurrentFrame(int index)
{
   //no need to update yet
   if(_currentFrame == index) return;

   //check direction and see if index
   //is valid
   if(_dir == 1){
      //low to high
      if(index >= _begin && index <= _end){
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

//add a child
void cfdSequence::addChild(cfdNode* child)
{
   //cout<<"Adding frame to sequence."<<endl;
   //init the switch node
   if(!_switch){
      _switch = new pfSwitch();
      _switch->setVal(PFSWITCH_OFF);
      
      //add the switch node to the tree
      pfGroup::addChild(_switch);
   }
   
   // Need to fix
   // fix this by adding virtual GetRaw node to cfdSceneNode
   //_switch->addChild(child);

   // force recomputation of time per frame
   this->setDuration( _duration );
}

//get the frame that is currently being displayed on the tree
int cfdSequence::getNextFrame()
{ 
   if(_currentFrame == _end && _lMode == CFDSEQ_CYCLE){
      return _begin;
   }else{
      //update the frame in the sequence
      return _currentFrame + _dir;
   }
}

void cfdSequence::setTime( double time )
{
   _deltaT = time;
   _duration = time * this->getNumChildren();
}

pfNode* cfdSequence::GetRawNode( void )
{
   return this;
}
