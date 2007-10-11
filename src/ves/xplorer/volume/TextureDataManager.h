/*************** <auto-copyright.pl BEGIN do not edit this line> *************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> **************/
#ifndef JPG_TEXTURE_DATA_MANAGER_H_
#define JPG_TEXTURE_DATA_MANAGER_H_

#include <ves/xplorer/volume/SingletonDLL.h>
#include <ves/xplorer/volume/TextureData.h>
#include <ves/VEConfig.h>

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
#ifdef WIN32
   VE_TEXTURE_BASED_TEMPLATE_EXPORTS template class VE_TEXTURE_BASED_EXPORTS Singleton<TextureDataManager_t>;
#endif
   /// Typedef for the singleton TextureDataManager.
   typedef Singleton<TextureDataManager_t> TextureDataManager;
}
#endif
