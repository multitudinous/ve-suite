#include "cfdSequence.h"
#include <iostream>
#include <cmath>

#include <Performer/pf/pfNode.h>
#include <Performer/pf/pfSwitch.h>
#include <Performer/pf/pfTraverser.h>

using namespace std;

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

   //_func = cfdSequence::_switchFrame;
   setTravFuncs(PFTRAV_APP,switchFrame,0);
   setTravData(PFTRAV_APP,this);
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
   int deltaFrame =1;

   //the sequence node w/ all the desired state info
   cfdSequence* sequence = (cfdSequence*)userData;

   //sequence->_switch->setVal(PFSWITCH_ON);
   //return PFTRAV_CONT;

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
   if(appFrameRate >= seqRate){
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
   if ( pMode == CFDSEQ_STOP || pMode == CFDSEQ_PAUSE )
   {
      //don't change the scene graph
      cout<<"Stopped or paused!"<<endl;
      return PFTRAV_CONT;
   }

   //if we've made it here, we are either resuming
   //or starting so check the loop mode and direction
   //to update the graph accordingly

   //we're restarting so reset current frame 
   //in the sequence to the beginning
   if ( pMode == CFDSEQ_START )
   {
      cout<<"Starting sequence."<<endl;
      sequence->_currentFrame = begin;
      sequence->_switch->setVal(sequence->_currentFrame);

      //notify we are that we have started
      sequence->setPlayMode( CFDSEQ_PLAYING );
      return PFTRAV_CONT;
   }

   //////////////////////////////////////////////////////////////////
   //reverseForward: relative direction
   //
   //1 == forward through frames in the same direction as _dir
   //-1 == backward through frames in the opposite direction as _dir
   //
   //if sequence->_dir is 1(forward)
   //we are displaying frames from low to high index.
   //
   //if sequence->_dir is -1(backward)
   //we are displaying frames from high to low index.
   //
   //reverseForward is used to increment correctly
   //depending on _dir and loop mode(cycle or swing) 
   //////////////////////////////////////////////////////////////////

   int reverseForward = 1;

   //depending on cycle type, decide the frame
   //to display
   if(sequence->_switch){
      //display the frame 
      sequence->_switch->setVal(sequence->_currentFrame);

      //update the next frame to display
      switch (lMode){
         case 1:
            //cout<<"Swing mode"<<endl;
            if(sequence->_dir ==1){

               //(swing) so go back and forth through frames 
               if(sequence->_currentFrame == end  ){

                  //reverse through sequence 
                  //from the last frame
                  reverseForward = -1;

               }else if(sequence->_currentFrame == begin){
                  //proceed forward through sequence 
                  //from the first frame
                  reverseForward =1;
               }
            }else if(sequence->_dir ==-1){
               //(swing) so go back and forth 
               if(sequence->_currentFrame == begin  ){

                  //reverse through sequence 
                  //from the last frame
                  reverseForward = 1;

               }else if(sequence->_currentFrame == end){
                  //proceed forward through sequence 
                  //from the first frame
                  reverseForward = -1;
               }
            }
            break;

         case 0:
         default:
            //cout<<"Cycle mode"<<endl;
            //we're at the end so start over(cycle)
            if(sequence->_dir == 1){
               if(sequence->_currentFrame == end ){
                  sequence->_currentFrame = begin;
               }
            }else if(sequence->_dir == -1){
               if(sequence->_currentFrame == begin ){
                  sequence->_currentFrame = end;
               }
            }
            //always going forward through frames
            reverseForward = 1;
            break;
         };


         //reset the application's Frame counter
         sequence->_appFrame = 0;

         //update the frame in the sequence
         sequence->_currentFrame += (deltaFrame*reverseForward*sequence->_dir);
            
   
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
/////////////////////////////////////
//set the current frame for cluster// 
//implementation                   // 
/////////////////////////////////////
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

      //add the switch node to the tree
      pfGroup::addChild(_switch);
   }
  
   _switch->addChild(child);
}

