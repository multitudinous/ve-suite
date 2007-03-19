#ifndef JPG_DATABASE_H_
#define JPG_DATABASE_H_

#include "VE_Xplorer/TextureBased/Data.h"
#include "VE_Xplorer/TextureBased/DatabaseDriver.h"

#include <loki/Function.h>
#include <loki/Singleton.h>

#include <map>
#include <string>

#include "VE_Installer/include/VEConfig.h"

namespace VE_TextureBased
{
   /**
    * Implementation of the Database Singleton.  This class handles a single
    * connection to a database; it delegates most functionality out to a
    * DatabaseDriver.
    */
   class VE_TEXTURE_BASED_EXPORTS Database_t
   {
   public:

      /**
       * Default Ctor
       * Initializes the driver to NULL.
       */
      Database_t()
         : mCurrentDriver(NULL)
      {
      }

      /**
       * Dtor
       * Closes and deletes the current driver if necessary.
       */
      ~Database_t()
      {
         if (mCurrentDriver)
         {
            mCurrentDriver->close();
         }
      }
      
      /**
       * Opens a connection to the given database.
       *
       * @param   name     the name of the database to open a connection to.
       *
       * @return     true if succesful, false if not.
       */
      bool open(const std::string& name)
      {
         if (!mCurrentDriver)
         {
            return false;
         }
         return mCurrentDriver->open(name);
      }

      /**
       * Closes the connection to the database.
       */
      void close()
      {
         if (mCurrentDriver && mCurrentDriver->isOpen())
         {
            mCurrentDriver->close();
         }
      }

      /**
       * Executes a SQL statement
       *
       * @param   statement      the SQL statement to execute.
       *
       * @return     true if successful, false otherwise
       */
      bool execute(const std::string& statement)
      {
         if (mCurrentDriver && mCurrentDriver->isOpen())
         {
            return mCurrentDriver->execute(statement);
         }
         return false;
      }

      /**
       * Queries the open status of the database.
       *
       * @return     true if the database is open, false otherwise.
       */
      bool isOpen()
      {
         if (mCurrentDriver)
         {
            return mCurrentDriver->isOpen();
         }
         return false;
      }

      /**
       * Sets the current driver.
       *
       * @param   name     the name of the driver to use.
       *
       * @return     true if successful, false otherwise.
       */
      bool setDriver(const std::string& name)
      {
         if (mDriverMap.find(name) != mDriverMap.end())
         {
            if (mCurrentDriver)
            {
               mCurrentDriver->close();
               delete mCurrentDriver;
            }
            mCurrentDriver = mDriverMap[name]();
            return mCurrentDriver == NULL;
         }
         return false;
      }

      /**
       * Returns the name of the current driver.
       *
       * @return     the name of the current driver.
       */
      std::string getDriverName() const
      {
         if (mCurrentDriver)
         {
            return mCurrentDriver->getDriverName();
         }
         return "None";
      }

      /**
       * Returns the version of the current driver.
       *
       * @return     the version of the current driver.
       */
      std::string getDriverVersion() const
      {
         if (mCurrentDriver)
         {
            return mCurrentDriver->getDriverVersion();
         }
         return "N/A";
      }

      /// driver creation functors.
      typedef Loki::Function<DatabaseDriver*()> DriverCreator;

      /**
       * Registers a driver with the database.
       *
       * @param   name     the name of the driver.
       * @param   creator  the function used to create a new instance of the
       *                   driver.
       */
      void registerDriver(const std::string& name, 
                          const DriverCreator& creator)
      {
         mDriverMap[name] = creator;
      }

   private:

      /// the current driver in use.
      DatabaseDriver*                                 mCurrentDriver;

      /// the map of driver names to their creation functions.
      std::map<std::string, DriverCreator>            mDriverMap;
   };

   /// Create the Singleton via Loki::SingletonHolder.
   typedef Loki::SingletonHolder<Database_t> Database;
}

#endif
