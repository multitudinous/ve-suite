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
#include <ves/xplorer/volume/TextureDataManager.h>
#include <ves/xplorer/volume/Database.h>

#include <sstream>

/// The following is from the Loki::Singleton library.  This allows a windows
/// DLL to have a Singleton with exactly one instance.  This example is
/// taken from the Singleton test case in the Loki source tree, and the macro
/// MUST be called from a source file (as opposed to a header file) to work
/// correctly.
typedef Loki::SingletonHolder<VE_TextureBased::TextureDataManager_t> TextureDataManagerH;
LOKI_SINGLETON_INSTANCE_DEFINITION(TextureDataManagerH)

namespace VE_TextureBased
{
   TextureDataPtr
   TextureDataManager_t::getTextureData(const std::string& name) const
   {
      TextureDataPtr result;
      return result;
   }

   bool
   TextureDataManager_t::createTextureFromDatabase(const std::string& db,
                                                const std::string& simulation)
   {
      // Open the database.
      if (!Database::Instance().isOpen() && 
          !Database::Instance().open(db) )
      {
         return false;
      }
      // Hard code the driver to be SQLite3.
      if ("None" == Database::Instance().getDriverName())
      {
         Database::Instance().setDriver("SQLite3");
      }
      // Pull back the requested simulation.
      if (!Database::Instance().execute(
       "SELECT ID, Name, Origin_ID, Spacing_ID FROM Simulations WHERE Name = " +
            simulation + ";"))
      {
         return false;
      }
      // Get the results.
      std::vector< std::vector<DBValue> > sim_results = 
         Database::Instance().getResults();
      // Iterate through all possible simulations.
      std::vector< std::vector<DBValue> >::iterator itr;
      for (itr = sim_results.begin(); itr != sim_results.end(); ++itr)
      {
         // Check to see if the row has exactly 4 columns.
         if (itr->size() != 4)
         {
            return false;
         }
         int64_t* sim_id = (*itr)[0].GetPtr<int64_t>();
         std::string* name = (*itr)[1].GetPtr<std::string>();
         int64_t* origin_id = (*itr)[2].GetPtr<int64_t>();
         int64_t* spacing_id = (*itr)[3].GetPtr<int64_t>();
         // Ensure that the values were correctly converted.
         // The origin is allowed to be NULL since not all datasets have an
         // origin.
         if (!sim_id || !name || !spacing_id)
         {
            return false;
         }
         // Find all vector values.
         VectorDataSet vds;
         std::ostringstream os;
         os << "SELECT Name, X, Y, Z, Timestep FROM Vectors WHERE "
            << "Simulation_ID = " << (*sim_id) << ";";
         if (!Database::Instance().execute(os.str()))
         {
            return false;
         }
         std::vector< std::vector<DBValue> > vrs = 
            Database::Instance().getResults();
         std::vector< std::vector<DBValue> >::iterator vitr;
         for (vitr = vrs.begin(); vitr != vrs.end(); ++vitr)
         {
            if (vitr->size() != 5)
            {
               return false;
            }
            std::string* vname = (*vitr)[0].GetPtr<std::string>();
            double* x = (*vitr)[1].GetPtr<double>();
            double* y = (*vitr)[2].GetPtr<double>();
            double* z = (*vitr)[3].GetPtr<double>();
            int64_t* ts = (*vitr)[4].GetPtr<int64_t>();
            if (!vname || !x || !y || !z || !ts)
            {
               return false;
            }
            vds.setVector(*vname, *ts, gmtl::Vec3f(*x, *y, *z));
         }
         // Find all of the scalar values.
         ScalarDataSet sds;
         os.clear();
         os << "SELECT Name, Value, Timestep FROM Scalars WHERE Simulation_ID"
            << " = " << (*sim_id) << ";";
         if (!Database::Instance().execute(os.str()))
         {
            return false;
         }
         std::vector< std::vector<DBValue> > srs = 
            Database::Instance().getResults();
         std::vector< std::vector<DBValue> >::iterator sitr;
         for (sitr = srs.begin(); sitr != srs.end(); ++sitr)
         {
            if (sitr->size() != 3)
            {
               return false;
            }
            std::string* sname = (*sitr)[0].GetPtr<std::string>();
            double* value = (*sitr)[1].GetPtr<double>();
            int64_t* ts = (*sitr)[2].GetPtr<int64_t>();
            if (!name || !value || !ts)
            {
               return false;
            }
            sds.setScalar(*sname, *ts, *value);
         }
         // Find the spacing vector.
         os.clear();
         os << "SELECT X, Y, Z FROM Vectors WHERE ID = "
            << (*spacing_id) << ";";
         if (!Database::Instance().execute(os.str()))
         {
            return false;
         }
         std::vector< std::vector<DBValue> > sprs = 
            Database::Instance().getResults();
         if (sprs.size() != 1 || sprs[0].size() != 3)
         {
            return false;
         }
         double* sx = sprs[0][0].GetPtr<double>(); 
         double* sy = sprs[0][1].GetPtr<double>(); 
         double* sz = sprs[0][2].GetPtr<double>(); 
         if (!sx || !sy || !sz)
         {
            return false;
         }
         gmtl::Vec3f spacing(*sx, *sy, *sz);
         gmtl::Vec3f* origin(0);
         // Find the origin vector, if it exists.
         if (origin_id)
         {
            os.clear();
            os << "SELECT X, Y, Z FROM Vectors WHERE ID = " << (*origin_id)
               << ";";
            if (!Database::Instance().execute(os.str()))
            {
               return false;
            }
            std::vector< std::vector<DBValue> > orrs =
               Database::Instance().getResults();
            if (orrs.size() != 1 || orrs[0].size() != 3)
            {
               return false;
            }
            double *ox = orrs[0][0].GetPtr<double>(); 
            double *oy = orrs[0][1].GetPtr<double>(); 
            double *oz = orrs[0][2].GetPtr<double>(); 
         }
         // Create the texture and add it to this manager.
         TextureDataPtr tdp = new TextureData(*name, vds, sds, spacing, origin);
         mTextureDataMap.insert(std::make_pair(*name, tdp));
      }
      return true;
   }

   template<>
   TextureDataManager_t& Singleton<TextureDataManager_t>::Instance()
   {
      return Loki::SingletonHolder<TextureDataManager_t>::Instance();
   }
   // Windows requires special measures for Singletongs and DLLs.
#ifdef _WIN32
   template class Singleton<TextureDataManager_t>;
#endif
}
template class Loki::Singleton<VE_TextureBased::TextureDataManager_t>;
