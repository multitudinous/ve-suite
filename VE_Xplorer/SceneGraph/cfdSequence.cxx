/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
#include <iostream>
#include "VE_Xplorer/SceneGraph/cfdSequence.h"
#include "VE_Xplorer/SceneGraph/cfdNode.h"
#include "VE_Xplorer/SceneGraph/cfdSwitch.h"
#include "VE_Xplorer/XplorerHandlers/cfdDebug.h"


#ifdef _PERFORMER
#include <Performer/pf.h>
#include <Performer/pf/pfNode.h>
//Performer static member for performer compliance
//it allows performer to determine the class type


#include <Performer/pf/pfSwitch.h>
#include <Performer/pf/pfTraverser.h>
pfType* VE_SceneGraph::cfdSequence::_classType = NULL;
//initialize our class w/ performer at run time
void VE_SceneGraph::cfdSequence::init(void)
{
   if(_classType == 0)
   {
      //initialize the parent
      pfGroup::init();
      //create the new class type
      _classType = new pfType(pfGroup::getClassType(),"cfdSequence");
   }
}
#elif _OSG
#include <osg/Node>
#include <osg/Group>
#include <osg/Switch>
#include <osg/FrameStamp>
#endif

using namespace VE_SceneGraph;
////////////////////////////////////////
//our class implementation
//////////////////////////
//Constructors          //
//////////////////////////
cfdSequence::cfdSequence():
#ifdef _PERFORMER
pfGroup(),
#elif _OSG
osg::Group(),
#endif
cfdGroup()
{
   _lSwitch = 0;
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
   
#ifdef _PERFORMER
   setTravFuncs(PFTRAV_APP,switchFrame,0);
   setTravData(PFTRAV_APP,this);
   setCopyFunc(copySequence);
   //performer stuff
   init();
   setType(_classType);
   // This needs to be a unique name per cfdsequence
   // this could be accomplished by encoding the time into
   // the name
   //setName("cfdSequence");
   /*if ( _group != NULL )
   {
      pfDelete( _group );
      _group = NULL;
   }*/
#elif _OSG
   setUpdateCallback(new cfdSequence::cfdSequenceCallback(this));
#endif
   SetCFDNodeType(CFD_SEQUENCE);
}
#ifdef _PERFORMER
///////////////////////////////////////////////////
cfdSequence::cfdSequence(const cfdSequence& cfdSeq)
:pfGroup(cfdSeq),
cfdGroup(cfdSeq)
{
   _lSwitch = new cfdSwitch(*cfdSeq._lSwitch);
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
//#ifdef _PERFORMER
   setTravFuncs(PFTRAV_APP,switchFrame,0);
   setTravData(PFTRAV_APP,this);
   //performer stuff
   init();
   setType(_classType);
   //_group = dynamic_cast<cfdSequence*>(_sequence);
//#elif _OSG
//#endif
   setCopyFunc(copySequence);
   SetCFDNodeType(CFD_SEQUENCE);
   //_sequence = this;
}
#endif
///////////////////////////
//Destructor             //
///////////////////////////
cfdSequence::~cfdSequence()
{
}
#ifdef _PERFORMER
///////////////////////////////////////////////////////////
cfdSequence& cfdSequence::operator=(const cfdSequence& rhs)
{
   if ( this != &rhs){
      //call parents = operator
#ifdef _PERFORMER
      pfGroup::operator =(rhs);
#elif _OSG
      //what is going on here
      //how does this work
      //osg::Group::operator=(rhs);
#endif 
      cfdGroup::operator =(rhs);

      //update everything
      _lSwitch = rhs._lSwitch;
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
#ifdef _PERFORMER
      //_sequence = rhs._sequence;
      setTravFuncs(PFTRAV_APP,switchFrame,0);
      setTravData(PFTRAV_APP,this);
      setCopyFunc(copySequence);
//      _group = dynamic_cast<cfdSequence*>(_sequence);
#elif _OSG
#endif
      SetCFDNodeType(CFD_SEQUENCE);
   }
   return *this;
}
///////////////////////////////////////////////////////////////////
void VE_SceneGraph::copySequence(pfObject* copy,
                       const pfObject* orig)
{
   std::cout<<"VE_SceneGraph::copySequence called!!"<<std::endl;
}
#elif _OSG
///////////////////////////////////////////////////////////
cfdSequence::cfdSequence(const cfdSequence& cfdSeq,
                  const osg::CopyOp& copyop)
:osg::Group(cfdSeq,copyop) 
{
   _lSwitch = new cfdSwitch(*cfdSeq._lSwitch);
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

   SetCFDNodeType(CFD_SEQUENCE);   
}
#endif
////////////////////////////////////////////////
void cfdSequence::setDuration( double duration )
{
   _duration = duration;
   _deltaT = duration / this->GetNumChildren();
}
//////////////////////////////////////////
void cfdSequence::setLoopMode( int lMode )
{
   _lMode = lMode;
}
////////////////////////////////////////
void cfdSequence::setPlayMode(int pMode)
{
   _pMode = pMode;
}
///////////////////////////
int cfdSequence::getStep()
{
   return _step;
}
////////////////////////////////
void cfdSequence::stepSequence()
{
   _step = CFDSEQ_STEP;
}
/////////////////////////////////
int cfdSequence::GetNumChildren()
{
   if ( _lSwitch )
   {
      return _lSwitch->GetNumChildren();
   }
   return -1;
}
////////////////////////////////////////////
int cfdSequence::RemoveChild(cfdNode* child)
{
   if ( _lSwitch )
   {
      return _lSwitch->RemoveChild(child);
   }
   return -1;
}
////////////////////////////////////////////
int cfdSequence::SearchChild(cfdNode* child)
{
   if(_lSwitch){
      return _lSwitch->SearchChild(child);   
   }
   return -1;
}
/////////////////////////////////////////
cfdNode* cfdSequence::GetChild(int index)
{
   if(_lSwitch){
      //cout<<"getting child: "<<index<<endl;
      return _lSwitch->GetChild(index);
   }
   return 0;
}
#ifdef _PERFORMER
/////////////////////////////////////////////////////////////////
//the pre node traverser callback to decide which frame to show//
/////////////////////////////////////////////////////////////////
int VE_SceneGraph::switchFrame(pfTraverser* trav, void* userData)
{
   //cout<<"Traversing cfdSequence node."<<endl;
   vprDEBUG(vesDBG,3) << "====cfdSequence::switchFrame()===="
                          << std::endl << vprDEBUG_FLUSH;

   //the sequence node w/ all the desired state info
   cfdSequence* sequence = (cfdSequence*)userData;
   
   //the number of frames
   int nChildren = sequence->GetSwitch()->GetNumChildren();
   //cout<<"Number of frames: "<<nChildren<<endl;
   vprDEBUG(vesDBG,3) << "Number of frames:"<<nChildren
                          << std::endl << vprDEBUG_FLUSH;
   

   //the sequence interval params
   int begin = sequence->getBegin();
   //cout<<"Beginning :"<<begin<<endl;
   vprDEBUG(vesDBG,3) << "Beginning frame: "<<begin
                          << std::endl << vprDEBUG_FLUSH;
   
   int end = sequence->getEnd();
   vprDEBUG(vesDBG,3) << "Ending frame: "<<end
                          << std::endl << vprDEBUG_FLUSH;
   
   //cout<<"End :"<<end<<endl;

   //the loop mode
   int lMode = sequence->getLoopMode();
   //cout<<"Loop Mode: "<<lMode<<endl;
    vprDEBUG(vesDBG,3) << "Loop Mode: "<<lMode
                          << std::endl << vprDEBUG_FLUSH;
 
   //the play mode(stop,start,pause,resume,playing)
   int pMode = sequence->getPlayMode();
   //cout<<"Play Mode: "<<pMode<<endl;  
    vprDEBUG(vesDBG,3) << "Play mode: "<<pMode
                          << std::endl << vprDEBUG_FLUSH;

   //length of sequence (secs)
   double duration = sequence->getDuration();

   //the frame rate the app is running at
   double appFrameRate = pfGetFrameRate();

   //the ratio of application frame rate 
   //to the desired sequence rate
   double frameRateRatio = 0;
   
   //make sure we have a valid interval 
   if(sequence->getDirection() ==1){
      if(end >  nChildren - 1){
         end = nChildren -1;
      }

      if(begin < 0){
        begin = 0;
      }
   }else if(sequence->getDirection() == -1){
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
   vprDEBUG(vesDBG,3) << "Sequence rate: "<<seqRate
                          << std::endl << vprDEBUG_FLUSH;
   //if we haven't passed enough frames in 
   //the app we don't need to update yet

   //the app is processing faster than sequence
   if(appFrameRate >= seqRate && pMode != CFDSEQ_START){
      //cout<<"Frame Rate: "<<appFrameRate<<endl;
      vprDEBUG(vesDBG,3) << "Performer Frame rate: "<<appFrameRate
                          << std::endl << vprDEBUG_FLUSH;
      //calculate frame rate ratio
      frameRateRatio = appFrameRate/seqRate;

      //since we're running slower than the
      //application, we don't need to update yet
      if(sequence->getAppFrameRate() < frameRateRatio)
      {
         vprDEBUG(vesDBG,3) << "Not time to switch frame yet due to slow frame rate: "<<appFrameRate
                          << " : " << sequence->getAppFrameRate() << std::endl << vprDEBUG_FLUSH;
         //continue in the current state 
         //update the frames the app has processed
         int curFrameRate = sequence->getAppFrameRate();
         curFrameRate++;
         sequence->setAppFrameRate(curFrameRate);
         //cout<<"App frames: "<<sequence->_appFrame<<endl;
         vprDEBUG(vesDBG,3) << "App frame total: "<<sequence->getAppFrameRate()
                          << std::endl << vprDEBUG_FLUSH;
         return PFTRAV_CONT;
      }
   }
   else if(appFrameRate < seqRate)
   {
      vprDEBUG(vesDBG,3) << "Slow Performer Frame rate: "<<appFrameRate
                          << std::endl << vprDEBUG_FLUSH;
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
       vprDEBUG(vesDBG,3) << "Stopped or paused the sequence "<<end
                          << std::endl << vprDEBUG_FLUSH;
      return PFTRAV_CONT;
   }

   //if we've made it here, we are either resuming
   //or starting so check the loop mode and direction
   //to update the graph accordingly

   //we're restarting so reset current frame 
   //in the sequence to the beginning
   if ( pMode == CFDSEQ_START ){
      //cout<<"Starting sequence."<<endl;
       vprDEBUG(vesDBG,3) << "Starting sequence. "<<end
                          << std::endl << vprDEBUG_FLUSH;
      sequence->setCurrentFrame(begin);
      sequence->GetSwitch()->SetVal(sequence->getCurrentFrame());

      //notify we are that we have started
      sequence->setPlayMode( CFDSEQ_PLAYING );
      return PFTRAV_CONT;
   }

   //depending on cycle type, decide the frame to display
   if(sequence->GetSwitch()){
      //get the next frame to display
      sequence->setCurrentFrame(sequence->getNextFrame());  
      
      //set the displayed frame on the switch
      sequence->GetSwitch()->SetVal(sequence->getCurrentFrame());

      //handle swing loop mode by changing directions
      //when we get to the beginning or end of the sequence
      if(lMode == CFDSEQ_SWING){
         //(swing) so go back and forth through frames 
         if(sequence->getCurrentFrame() == end|| 
            sequence->getCurrentFrame() == begin){
	         //switch the direction of the sequence
            int curDirection = sequence->getDirection(); 
            curDirection *= -1;
            sequence->setDirection(curDirection);
         }
      }
      //reset the appFrame counter for synchronization
      sequence->setAppFrameRate(0);

      //if we are stepping, pause the sequence
      if(sequence->getStep() == CFDSEQ_STEP){
         sequence->setPlayMode(CFDSEQ_PAUSE);
         sequence->setStep(0) ;
      }
   }
    vprDEBUG(vesDBG,3) << "Set current frame to frame number: "<<sequence->getCurrentFrame()
                          << std::endl << vprDEBUG_FLUSH;
   return PFTRAV_CONT;
} 
#elif _OSG
//////////////////////////////////////////////////////////
cfdSequence::cfdSequenceCallback::cfdSequenceCallback(cfdSequence* seq)
{
   _sequence = seq;
   _prevTime = 0;
   _prevFrame = 0;
}
///////////////////////////////////////////////////////////////////////////
void cfdSequence::cfdSequenceCallback::operator()(osg::Node* node, 
                                             osg::NodeVisitor* nv)
{
   
   vprDEBUG(vesDBG,3) << "cfdSequence::switchFrame"
                          << std::endl << vprDEBUG_FLUSH;
   //the number of frames
   int nChildren = _sequence->_lSwitch->GetNumChildren();
   vprDEBUG(vesDBG,3) << "Number of frames:"<<nChildren
                          << std::endl << vprDEBUG_FLUSH;
 //the sequence interval params
   int begin = _sequence->_begin;
   //cout<<"Beginning :"<<begin<<endl;
   vprDEBUG(vesDBG,3) << "Beginning frame: "<<begin
                          << std::endl << vprDEBUG_FLUSH;
   
   int end = _sequence->_end;
   vprDEBUG(vesDBG,3) << "Ending frame: "<<end
                          << std::endl << vprDEBUG_FLUSH;
   
   //cout<<"End :"<<end<<endl;

   //the loop mode
   int lMode = _sequence->_lMode;
   //cout<<"Loop Mode: "<<lMode<<endl;
    vprDEBUG(vesDBG,3) << "Loop Mode: "<<lMode
                          << std::endl << vprDEBUG_FLUSH;
 
   //the play mode(stop,start,pause,resume,playing)
   int pMode = _sequence->_pMode;
   //cout<<"Play Mode: "<<pMode<<endl;  
    vprDEBUG(vesDBG,3) << "Play mode: "<<pMode
                          << std::endl << vprDEBUG_FLUSH;


   //length of _sequence (secs)
   double duration = _sequence->_duration;
   vprDEBUG(vesDBG,3) << "Duration: "<<duration
                          << std::endl << vprDEBUG_FLUSH;

   //the frame rate the app is running at
   double currTime =0;
   int frameNumber = 0;
   double appFrameRate = 0;
   if(nv->getFrameStamp()){
     currTime = nv->getFrameStamp()->getReferenceTime();
     frameNumber = nv->getTraversalNumber();
     appFrameRate = (double)(frameNumber-_prevFrame)/(currTime-_prevTime);
     _prevTime = currTime;
     _prevFrame = frameNumber;
   }else{
      std::cout<<"If u see this, we need to set the framestamp on the callback"<<std::endl;
      std::cout<<"at init time in the constructor. . ."<<std::endl;
   }

   //the ratio of application frame rate 
   //to the desired _sequence rate
   double frameRateRatio = 0;
   
   //make sure we have a valid interval 
   if(_sequence->_dir ==1){
      if(end >  nChildren - 1){
         end = nChildren -1;
      }

      if(begin < 0){
        begin = 0;
      }
   }else if(_sequence->_dir == -1){
      if(begin >  nChildren - 1){
         begin = nChildren -1;
      }

      if(end < 0){
        end = 0;
      }
   }

   //check if we need to update the _sequence node 

   //the desired rate of the _sequence (fps)
   double seqRate = fabs((double)(begin - end))/duration;
   //cout<<"Sequence rate: "<<seqRate<<endl;

   //if we haven't passed enough frames in 
   //the app we don't need to update yet

   //the app is processing faster than _sequence
   if(appFrameRate >= seqRate && pMode != CFDSEQ_START){
      //cout<<"Frame Rate: "<<appFrameRate<<endl;
      //calculate frame rate ratio
      frameRateRatio = appFrameRate/seqRate;

      //since we're running slower than the
      //application, we don't need to update yet
      if(_sequence->_appFrame < frameRateRatio){
         //continue in the current state 
         //update the frames the app has processed
         _sequence->_appFrame++;
         //cout<<"App frames: "<<_sequence->_appFrame<<endl;
         
         //traverse the switch
         traverse(node,nv);
         return;
      }
       //cout<<"Switching frames!"<<endl;
   }else if(appFrameRate < seqRate){
      //cout<<"Frame rate is slow."<<endl;
      //app is running slower than the _sequence
      //need to skip some frames in the _sequence
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
      vprDEBUG(vesDBG,3) << "Stopped or paused the sequence "<<end
                          << std::endl << vprDEBUG_FLUSH;
      //traverse the swtich
      traverse(node,nv);
      return;
   }

   //if we've made it here, we are either resuming
   //or starting so check the loop mode and direction
   //to update the graph accordingly

   //we're restarting so reset current frame 
   //in the _sequence to the beginning
   if ( pMode == CFDSEQ_START ){
      //cout<<"Starting _sequence."<<endl;
      vprDEBUG(vesDBG,3) << "Starting the sequence "<<end
                          << std::endl << vprDEBUG_FLUSH;
      _sequence->_currentFrame = begin;
      _sequence->_lSwitch->SetVal(_sequence->_currentFrame);

      //notify we are that we have started
      _sequence->setPlayMode( CFDSEQ_PLAYING );
      //traverse the node
      traverse(node,nv);
      return;
   }

   //depending on cycle type, decide the frame to display
   if(_sequence->_lSwitch){
      //get the next frame to display
      _sequence->_currentFrame = _sequence->getNextFrame();
      
      //set the displayed frame on the switch
      _sequence->_lSwitch->SetVal(_sequence->_currentFrame);

      //handle swing loop mode by changing directions
      //when we get to the beginning or end of the _sequence
      if(lMode == CFDSEQ_SWING){
         //(swing) so go back and forth through frames 
         if(_sequence->_currentFrame == end|| 
            _sequence->_currentFrame == begin){
	         //switch the direction of the _sequence
            _sequence->_dir *= -1;
         }
      }
      //reset the appFrame counter for synchronization
      _sequence->_appFrame = 0.0f;

      //if we are stepping, pause the _sequence
      if(_sequence->_step == CFDSEQ_STEP){
         _sequence->_pMode = CFDSEQ_PAUSE;
         _sequence->_step = 0;
      }
   }
   vprDEBUG(vesDBG,3) << "Set current frame to frame number: "<<_sequence->_currentFrame
                          << std::endl << vprDEBUG_FLUSH;

   //traverse the node
   traverse(node,nv);
} 
#endif
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
#ifdef _PEFORMER
	      _lSwitch->SetVal(_currentFrame);
#elif _OSG
#endif
      }else{
         //invalid index
         std::cout<<"Error: cfdSequence!"<<std::endl;
         std::cout<<"Frame index: "<<index
             <<" is outside of range of valid frames!"<<std::endl;
         return;
      }
   }else if(_dir == -1){
      //high to low
      if(index >= _end && index < _begin){
         _currentFrame = index;
	 _lSwitch->SetVal(_currentFrame);
      }else{
         //invalid index
         std::cout<<"Error: cfdSequence!"<<std::endl;
         std::cout<<"Frame index: "<<index
             <<" is outside of range of valid frames!"<<std::endl;
         return;
      }
   }
   return; 
}
//////////////////////////////////////////
//add a child                           //
//////////////////////////////////////////
int cfdSequence::AddChild(cfdNode* child)
{
   //std::cout << "Adding frame to sequence." << std::endl;
   //init the switch node
   if ( !_lSwitch )
   {
      _lSwitch = new cfdSwitch();
      _lSwitch->SetVal(cfdSwitch::OFF);

      addChild(_lSwitch->GetRawNode());
   }
   _lSwitch->AddChild(child);

   // force recomputation of time per frame
   //this->setDuration( _duration );
   return 1;
}
///////////////////////////////
//get the frame that is      //
//currently being displayed  //
//on the tree                //
///////////////////////////////
int cfdSequence::getNextFrame()
{ 
   if(_currentFrame == _end && _lMode == CFDSEQ_CYCLE){
      return _begin;
   }else{
      //update the frame in the sequence
      return _currentFrame + _dir;
   }
}
////////////////////////////////////////
void cfdSequence::setTime( double time )
{
   _deltaT = time;
   _duration = time * this->GetNumChildren();
}
///////////////////////////////
// Reimplement for other graphs
#ifdef _PERFORMER
pfNode* cfdSequence::GetRawNode( void )
#elif _OSG
osg::Node* cfdSequence::GetRawNode(void)
#elif _OPENSG
#endif
{
   return (this);
}

