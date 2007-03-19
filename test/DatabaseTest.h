#ifndef JPG_DATABASE_TEST_H_
#define JPG_DATABASE_TEST_H_

#include "VE_Xplorer/TextureBased/Database.h"

#include <cxxtest/TestSuite.h>

class DatabaseTest : public CxxTest::TestSuite
{
public:
   void setUp()
   {
      db = &VE_TextureBased::Database::Instance();
      db->setDriver("SQLite3");
   }

   void testSetDriver()
   {
      db->setDriver("SQLite3");
      TS_ASSERT_EQUALS(db->getDriverName(), "SQLite3");
      TS_ASSERT_EQUALS(db->getDriverVersion(), "0.0.1");
   }

   void testOpen()
   {
      TS_ASSERT(db->open("test.db"));
   }

   void testClose()
   {
      db->open("test.db");
      TS_ASSERT(db->isOpen());
      db->close();
      TS_ASSERT_EQUALS(db->isOpen(), false);
   }

   void testExecute()
   {
      TS_ASSERT(db->open("test.db"));
      TS_ASSERT(db->execute("SELECT * FROM Users WHERE ID = 0;"))
      db->close(); 
   }
private:
   
   /// the instance of the datbase
   VE_TextureBased::Database_t*                                  db;
};

#endif
