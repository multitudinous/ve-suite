#ifndef AG_CIRCULAR_QUEUE_TEST_H_
#define AG_CIRCULAR_QUEUE_TEST_H_

#include "apps/voice/CircularQueue.h"

#include <cxxtest/TestSuite.h>

#include <string>

class CircularQueueTest : public CxxTest::TestSuite
{
public:

   void testAdd()
   {
      CircularQueue<std::string> queue;

      TS_ASSERT(queue.add("Foo"));
      TS_ASSERT(queue.add("Bar"));

      TS_ASSERT_EQUALS(queue.size(), 2);

      std::string r1;
      std::string r2;

      TS_ASSERT_EQUALS(queue.remove(r1), true);
      TS_ASSERT_EQUALS(queue.remove(r2), true);

      TS_ASSERT_EQUALS(r1, "Foo");
      TS_ASSERT_EQUALS(r2, "Bar");

      TS_ASSERT_EQUALS(queue.size(), 0);

      size_t cap = queue.capacity();

      for (size_t i = 0; i < cap; ++i)
      {
         TS_ASSERT(queue.add("Dummy"));
      }
      TS_ASSERT_EQUALS(queue.add("OVERFLOW"), false);
      TS_ASSERT(queue.isFull());
      TS_ASSERT_EQUALS(queue.size(), queue.capacity());
      TS_ASSERT(queue.remove(r1)); 
      TS_ASSERT_EQUALS(queue.size(), queue.capacity() - 1);
      TS_ASSERT_EQUALS(queue.isFull(), false);
      TS_ASSERT(queue.add("Smart"));
      TS_ASSERT(queue.isFull());
      TS_ASSERT_EQUALS(queue.size(), queue.capacity());
   }

   void testRemove()
   {
      CircularQueue<std::string> queue;

      TS_ASSERT(queue.add("Foo"));
      TS_ASSERT(queue.add("Bar"));

      TS_ASSERT_EQUALS(queue.size(), 2);

      std::string r1;
      std::string r2;

      TS_ASSERT(queue.remove(r1));
      TS_ASSERT(queue.remove(r2));

      TS_ASSERT_EQUALS(r1, "Foo");
      TS_ASSERT_EQUALS(r2, "Bar");

      TS_ASSERT_EQUALS(queue.size(), 0);

      TS_ASSERT_EQUALS(queue.remove(r1), false);
   }

   void testIsEmpty()
   {
      CircularQueue<std::string> queue;

      TS_ASSERT(queue.isEmpty());

      TS_ASSERT(queue.add("Foo"));
      TS_ASSERT_EQUALS(queue.isEmpty(), false);
      TS_ASSERT(queue.add("Bar"));
      TS_ASSERT_EQUALS(queue.isEmpty(), false);
      std::string r1;
      std::string r2;
      TS_ASSERT(queue.remove(r1));
      TS_ASSERT(queue.remove(r2));
      TS_ASSERT(queue.isEmpty());
   }

   void testIsFull()
   {
      CircularQueue<std::string> queue;

      TS_ASSERT_EQUALS(queue.isFull(), false);

      TS_ASSERT(queue.add("Foo"));
      TS_ASSERT_EQUALS(queue.isFull(), false);
      TS_ASSERT(queue.add("Bar"));
      TS_ASSERT_EQUALS(queue.isFull(), false);

      std::string r1;
      std::string r2;

      TS_ASSERT_EQUALS(queue.remove(r1), true);
      TS_ASSERT_EQUALS(queue.isFull(), false);
      TS_ASSERT_EQUALS(queue.remove(r2), true);
      TS_ASSERT_EQUALS(queue.isFull(), false);

      size_t cap = queue.capacity();

      for (size_t i = 0; i < cap; ++i)
      {
         TS_ASSERT(queue.add("Dummy"));
      }
      TS_ASSERT(queue.isFull());
   }

   void testFlush()
   {
      CircularQueue<std::string> queue;

      TS_ASSERT(queue.add("Foo"));
      TS_ASSERT(queue.add("Bar"));

      TS_ASSERT_EQUALS(queue.size(), 2);

      queue.flush();
      TS_ASSERT_EQUALS(queue.size(), 0);

      size_t cap = queue.capacity();

      for (size_t i = 0; i < cap; ++i)
      {
         TS_ASSERT(queue.add("Dummy"));
      }
      TS_ASSERT_EQUALS(queue.size(), cap);
      queue.flush();
      TS_ASSERT_EQUALS(queue.size(), 0);
      queue.flush();
      TS_ASSERT_EQUALS(queue.size(), 0);
   }
};

#endif
