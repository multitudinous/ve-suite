#ifndef JPG_SINGLETON_DLL_H_
#define JPG_SINGLETON_DLL_H_

#include "VE_Installer/include/VEConfig.h"

namespace VE_TextureBased
{
   /**
    * This little Singleton is necessary to 
    * make Singletons actually have a single instance in Windows DLLs.
    */
   template <class T>
   class VE_TEXTURE_BASED_EXPORTS Singleton
   {
   public:
      static T& Instance();
   };
}

#endif
