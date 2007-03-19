#ifndef JPG_DATABASE_DRIVER_H_
#define JPG_DATABASE_DRIVER_H_

#include "VE_Xplorer/TextureBased/Data.h"

#include <string>
#include <vector>

namespace VE_TextureBased 
{
   /**
    * The interface that all database drivers must adhere to.  A database 
    * driver can be thought of as both the bridge between an application and
    * the Database API and the cursor that the application uses to execute
    * queries and retrieve results.
    */
   class DatabaseDriver
   {
   public:

      virtual ~DatabaseDriver()
      {}

      /**
       * Opens a connection to the database via this driver.
       *
       * @param   name     the name of the database to open.
       *
       * @return     true if sucessful, false otherwise.
       */
      virtual bool open(const std::string& name) = 0;

      /**
       * Closes a connection to the database.
       */
      virtual void close() = 0;

      /**
       * Executes the given query.
       *
       * @param   statement   the SQL query statement to execute.
       *
       * @return     true if successful, false otherwise
       */
      virtual bool execute(const std::string& query) = 0;

      /**
       * Gets the results from the most recent query.
       *
       * @return     the results from the most recent query.
       */
      virtual std::vector< std::vector<DBValue> > getResults() const = 0;

      /**
       * Queries the open status of this database
       *
       * @return     true if the database is open, false otherwise.
       */
      virtual bool isOpen() const = 0;

      /**
       * Gets the name of this database driver.
       */
      virtual std::string getDriverName() const = 0;

      /**
       * Gets the version of this database driver.
       */
      virtual std::string getDriverVersion() const = 0;
   };
}
#endif
