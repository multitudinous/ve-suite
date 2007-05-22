#ifndef JPG_TEXTURE_DATA_MANAGER_H_
#define JPG_TEXTURE_DATA_MANAGER_H_

#include "VE_Xplorer/TextureBased/SingletonDLL.h"
#include "VE_Xplorer/TextureBased/TextureData.h"
#include "VE_Installer/include/VEConfig.h"

#define LOKI_SINGLETON_EXPORT VE_TEXTURE_BASED_EXPORTS
#include <loki/Singleton.h>

#ifdef _WIN32
#include <windows.h>
#define WIN32_LEAN_AND_MEAN
#endif
#include <iostream>
#include <map>
#include <vector>
#include <string>
namespace VE_TextureBased
{
   /**
    * This Singleton manages each simulation contained in a TextureData 
    * object.  It will create texture data objects on demand from a
    * database.
    */
   class VE_TEXTURE_BASED_EXPORTS TextureDataManager_t
   {
   public:

      TextureDataManager_t()
      {}
     
      /**
       * Retrieves the specified TextureData set.
       *
       * @param   name     the name of the texture data to retrieve.
       *
       * @return     a TextureDataPtr to the texture data.
       */
      TextureDataPtr getTextureData(const std::string& name) const; 

      /**
       * Creates TextureData from the specified database and Simulation name
       * If no simulation name is given, it will create TextureData from all
       * simulations in the database.
       *
       * @param   db             the name of the database to use.
       * @param   simualation    the name of the simulation to use.
       *
       * @return     true if successful, false if an error occurred.
       */
      bool createTextureFromDatabase(const std::string& db, 
                                     const std::string& simulation);

      /**
       * Add the given texture data to this manager.
       *
       * @param   data     the texture data to add.
       */
      void addTextureData(TextureData& data)
      {
         TextureDataPtr dp(&data);
         mTextureDataMap.insert(std::make_pair(data.getName(), dp));
      }

   private:

      /// The map of texture data set name's to their pointers.
      std::multimap<std::string, TextureDataPtr>        mTextureDataMap;
   };

   /// Typedef for the singleton declaration.  This is necessary to make the
   /// singleton have one instance in a Windows DLL; the macro calls are 
   /// based upon recommendations from the MSDN documentation.
   VE_TEXTURE_BASED_TEMPLATE_EXPORTS template class VE_TEXTURE_BASED_EXPORTS Singleton<TextureDataManager_t>;

   /// Typedef for the singleton TextureDataManager.
   typedef Singleton<TextureDataManager_t> TextureDataManager;
}
#endif
