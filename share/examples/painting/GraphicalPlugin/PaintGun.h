#ifndef PAINTGUN_H
#define PAINTGUN_H

#include "Gun.h"

// this class is meant to encompass the base Gun class, and extend it for things like specialized drawing,
// or anything else specific to a paint gun.

class PaintGun : public Gun
   {
   public:
      PaintGun() {}
      virtual ~PaintGun() {}

   };

#endif
