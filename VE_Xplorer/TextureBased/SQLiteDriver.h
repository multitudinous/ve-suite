#ifndef JPG_SQLITE_DRIVER_H_
#define JPG_SQLITE_DRIVER_H_

#include "VE_Xplorer/TextureBased/DatabaseDriver.h"
#include "VE_Xplorer/TextureBased/Data.h"

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
