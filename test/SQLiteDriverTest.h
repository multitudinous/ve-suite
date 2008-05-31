#ifndef JPG_SQLITE_DRIVER_TEST_H_
#define JPG_SQLITE_DRIVER_TEST_H_

#include "ves/xplorer/volume/SQLiteDriver.h"
#include "ves/xplorer/volume/Data.h"

#include <cxxtest/TestSuite.h>

#include <iostream>

class DBVisitor : public VE_TextureBased::DBValue::StrictVisitor
{
public:
   void Visit(VE_TextureBased::DBNullValue& v)
   {
      std::cout << "NULL ";
   }

   void Visit(int64_t& v)
   {
      std::cout << "Int64: " << v << " ";
   }
   
   void Visit(double& v)
   {
      std::cout << "Double: " << v << " ";
   }

   void Visit(std::string& v)
   {
      std::cout << "String: " << v << " ";
   }

   void Visit(VE_TextureBased::BinaryData& v)
   {
      std::cout << "Binary Data: ";
      for (size_t i = 0; i < v.getSize(); ++i)
      {
         std::cout.setf(std::ios_base::hex);
         std::cout << v[i] << " ";
      }
   }
};

class SQLiteDriverTest : public CxxTest::TestSuite
{
public:

   void testOpen()
   {
      VE_TextureBased::SQLiteDriver driver;
      TS_ASSERT(driver.open("test.db"));
      TS_ASSERT(driver.isOpen()); 
      driver.close();
      TS_ASSERT_EQUALS(driver.isOpen(), false);
   }

   void testClose()
   {
      VE_TextureBased::SQLiteDriver driver;
      driver.open("test.db");
      TS_ASSERT(driver.isOpen());
      driver.close();
      TS_ASSERT_EQUALS(driver.isOpen(), false);
   }

   void testExecute()
   {
      VE_TextureBased::SQLiteDriver driver;
      TS_ASSERT(driver.open("test.db"));
      TS_ASSERT(driver.execute("SELECT * FROM Users;"));
      std::vector< std::vector<VE_TextureBased::DBValue> > results;
      results = driver.getResults();
      TS_ASSERT_EQUALS(results.empty(), false);
      std::cout << std::endl;
      DBVisitor visitor;
      std::vector< std::vector<VE_TextureBased::DBValue> >::iterator row_itr;
      for(row_itr = results.begin(); row_itr != results.end(); ++row_itr)
      {
         std::vector<VE_TextureBased::DBValue>::iterator col_itr;
         for (col_itr = row_itr->begin(); col_itr != row_itr->end(); ++col_itr)
         {
            col_itr->Accept(visitor);            
         }
         std::cout << std::endl;
      }
   }

   void testIsOpen()
   {
      VE_TextureBased::SQLiteDriver driver;
      TS_ASSERT_EQUALS(driver.isOpen(), false);
      driver.open("test.db");
      TS_ASSERT(driver.isOpen());
      driver.close();
      TS_ASSERT_EQUALS(driver.isOpen(), false);
   }
};

#endif
