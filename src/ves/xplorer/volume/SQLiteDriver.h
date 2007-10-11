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
#ifndef JPG_SQLITE_DRIVER_H_
#define JPG_SQLITE_DRIVER_H_

#include <VE_Xplorer/TextureBased/DatabaseDriver.h>
#include <VE_Xplorer/TextureBased/Data.h>

#include <sqlite3.h>

#include <string>
#include <vector>

namespace VE_TextureBased 
{
   /**
    * Implementation of the database driver interface for SQLite databases.
    */
   class SQLiteDriver : public DatabaseDriver
   {
   public:

      /**
       * Default Ctor
       *
       * Initializes the connection to NULL.
       */
      SQLiteDriver()
         : mConnection(NULL), mOpen(false)
      {}

      /**
       * Dtor
       * Cleans up the connection.
       */
      ~SQLiteDriver()
      {
         if (mConnection)
         {
            sqlite3_close(mConnection); 
            mConnection = NULL;
         }
      }

      /// Virtual Overrides
      bool open(const std::string& name);

      void close();

      bool execute(const std::string& query);

      bool isOpen() const
      {
         return mOpen;
      }

      std::vector< std::vector< DBValue > > getResults() const
      {
         return mResults;
      }

      DatabaseDriver* create()
      {
         return new SQLiteDriver();
      }

      std::string getDriverName() const
      {
         return "SQLite3";
      }

      std::string getDriverVersion() const
      {
         return "0.0.1";
      }

   private:

      /// the connection to the sqlite database.
      sqlite3*                                     mConnection;

      /// the status of the connection (open or closed)
      bool                                         mOpen;

      /// the name of the database currently open.
      std::string                                  mName;

      /// the results of the most recently executed query.
      std::vector< std::vector< DBValue > >        mResults;
   };
}

#endif
