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
