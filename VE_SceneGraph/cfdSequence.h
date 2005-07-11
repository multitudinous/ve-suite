/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2005 by Iowa State University
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

enum cfdPlayMode
{
   CFDSEQ_STOP = 0,
   CFDSEQ_START,
   CFDSEQ_PAUSE,
   CFDSEQ_RESUME,
   CFDSEQ_PLAYING,   // not in pfSequence
   CFDSEQ_STEP       // not in pfSequence
};

enum cfdLoopMode
{
   CFDSEQ_CYCLE = 0,
   CFDSEQ_SWING
};

#include "VE_SceneGraph/cfdGroup.h"
namespace VE_SceneGraph{
   class cfdNode;
   class cfdSwitch;
}

#ifdef _PERFORMER
class pfNode;
class pfType;
class pfSwitch;
class pfTraverser;
#include <Performer/pf/pfGroup.h>
#elif _OSG
#include <osg/Group>
namespace { class Switch; }
#endif

namespace VE_SceneGraph{
#ifdef _PERFORMER
   class VE_SCENEGRAPH_EXPORTS cfdSequence : public pfGroup, public cfdGroup
#elif _OSG
   class VE_SCENEGRAPH_EXPORTS cfdSequence : public osg::Group, public cfdGroup
#endif
   {
      public:
         cfdSequence();
         virtual ~cfdSequence();
#ifdef _PERFORMER
         cfdSequence(const cfdSequence& cfdSeq);
         //to make this a performer class
         static void init(void);

         static pfType* getClassType( void ){ return _classType; }
         cfdSequence& operator=(const cfdSequence& rhs);
         //virtual int copy(const cfdSequence* copy);
         //void copy(cfdSequence* copy,const cfdSequence* orig);

         
         
         //virtual cfdSequence* cfdSequence::clone(int);
         
#elif _OSG
         cfdSequence(const cfdSequence& pbQuad,
                        const osg::CopyOp& copyop=osg::CopyOp::SHALLOW_COPY);
         META_Object(VE_SceneGraph,cfdSequence);
#endif   
         // set/get the duration (in seconds) of any particular frame
         void setTime( double time );
         double getTime(){ return _deltaT; }
  
         // set/get the duration (in seconds) of the entire sequence
         void setDuration( double duration );
         double getDuration(){ return _duration; }

         // set/get the interval
         void setInterval( int loopmode, int begin, int end );
         int getBegin(){ return _begin; }
         int getEnd(){ return _end; }

         // set/get the loop mode of the sequence
         void setLoopMode( int lMode );
         int getLoopMode(){ return _lMode; }

         // set/get the stop/start/pause/resume 
         void setPlayMode( int pMode );
         int getPlayMode(){ return _pMode; }

         // set/get the current frame
         void setCurrentFrame( int frameIndex );
         int getCurrentFrame(){ return _currentFrame; }

         // step the sequence one frame in a particular direction
         void stepSequence();

         //get the direction of the sequence
         int getDirection(){ return _dir; }

         //change the direction of the frame viewing
         //if it is currently foward(low->high index)
         //change to reverse(high->low index) and vice versa
         void changeSequenceDirection();

         //set the direction explicitly
         //1 == positive direction
         //-1 == negative direction
         void setDirection(int posNeg){_dir = posNeg;}

         //get the next frame index
         int getNextFrame();

         //get number of children
         virtual int GetNumChildren();

         //add a child node
         virtual int AddChild( cfdNode* child );

         //get the index of a child
         virtual int SearchChild( cfdNode* child );

         //get the specified child node  
         virtual cfdNode* GetChild( int index );

         //remove child 
         virtual int RemoveChild( cfdNode* child );

         double getAppFrameRate(){return _appFrame;}
         void setAppFrameRate(double afr){_appFrame = afr;}

         void setStep(int stepMode){_step = stepMode;}
         int getStep();
#ifdef _PERFORMER
         //the node pre-traverser callback
         friend int switchFrame(pfTraverser* trav, void* userData);
         friend void copySequence(pfObject* copy,
                        const pfObject* orig);
#elif _OSG
         //callbacks are defined as classes in OSG!!!
         class cfdSequenceCallback : public osg::NodeCallback{
            public:
              cfdSequenceCallback(cfdSequence* seq);
               virtual void operator()(osg::Node* node, osg::NodeVisitor* nv);
        
           protected:
               virtual ~cfdSequenceCallback(){}
               cfdSequence* _sequence;
               double _prevTime;
               int _prevFrame;
         };

#endif
#ifdef _PERFORMER
        pfNode* GetRawNode( void );
        cfdSwitch* GetSwitch(){return _lSwitch;}
#elif _OSG
         osg::Node* GetRawNode( void );
#elif _OPENSG
#endif
protected:
         cfdSwitch* _lSwitch;
         double _appFrame;
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
#ifdef _PERFORMER
         static pfType* _classType;
#elif _OSG
#endif
   };
}

#endif //_VRAC_CFD_SEQUENCE_H_
