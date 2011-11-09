#ifndef GUNINPUTADAPTERKEYBOARD_H
#define GUNINPUTADAPTERKEYBOARD_H

#include <osgGA/GUIEventHandler>

#include "PaintGun.h"

// this class handles keyboard input and modifies the paint gun object accordingly.
// it will also update the HUD when the gun paramaters change.

class GunInputAdapterKeyboard : public osgGA::GUIEventHandler
   {
   public:
      GunInputAdapterKeyboard(PaintGun *pg) {_gun = pg;}

      virtual bool handle(const osgGA::GUIEventAdapter &ea, osgGA::GUIActionAdapter &aa);

      void KeyPress(const osgGA::GUIEventAdapter &ea);
      void KeyRelease(const osgGA::GUIEventAdapter &ea);

   private:
      GunInputAdapterKeyboard() {}  // can't make this object without passing passing a pointer to a gun.

      osg::ref_ptr<PaintGun> _gun;
   };

#endif