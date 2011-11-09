#ifndef GUNINPUTADAPTERMOUSE_H
#define GUNINPUTADAPTERMOUSE_H

// Name: VirtualPaint Demo
// GunInputAdapterMouse.h
//
// Authors:
//   Chris 'Xenon' Hanson, AlphaPixel.
//   John Enright, Digital Transforms.
//
// Origination Date: July 19, 2011

#include <algorithm>
#include <osgViewer/ViewerEventHandlers>

#include "PaintGun.h"

// I'd like to refactor this out into a utility templates header
template <typename T> T CLAMP(T value, T low, T high)
{
	return std::min( high, std::max(low,value) );
}

// --------------------------------------------------------------------------
// Define an event handler for tracking mouse movement. This is used to move
// the custom mouse cursor / pointer.

class MouseTracker : public osgGA::GUIEventHandler
{
public:
	virtual bool handle(const osgGA::GUIEventAdapter &ea, osgGA::GUIActionAdapter &aa, osg::Object *object, osg::NodeVisitor *visitor);
};

// --------------------------------------------------------------------------
// this feeds mouse input to the gun.

class GunInputAdapterMouse : public osgGA::GUIEventHandler
{
   public:
	   GunInputAdapterMouse(PaintGun *pg) : _buttonTriggerPullAmount(1.0) {_gun = pg;}

	   virtual bool handle(const osgGA::GUIEventAdapter &ea, osgGA::GUIActionAdapter &aa);
	   void setButtonTriggerPullAmount(float newValue) {_buttonTriggerPullAmount = CLAMP(newValue, 0.0f, 1.0f);}
	   float getButtonTriggerPullAmount(void) const {return(_buttonTriggerPullAmount);}
	   void incButtonTriggerPullAmount(void) {setButtonTriggerPullAmount(getButtonTriggerPullAmount() + 0.1f);};
	   void decButtonTriggerPullAmount(void) {setButtonTriggerPullAmount(getButtonTriggerPullAmount() - 0.1f);};


   private:
      GunInputAdapterMouse() {}     // can't make this object without passing passing a pointer to a gun.

      osg::ref_ptr<PaintGun> _gun;
	  float _buttonTriggerPullAmount;
};

#endif