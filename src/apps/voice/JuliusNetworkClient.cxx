
#include "apps/voice/JuliusNetworkClient.h"

#include <ace/INET_Addr.h>
#include <ace/Time_Value.h>
#include <ace/os_include/sys/os_uio.h>

JuliusNetworkClient::JuliusNetworkClient()
   : mParser(NULL), mConnected(false)
{
}

JuliusNetworkClient::~JuliusNetworkClient()
{
   if (mConnected)
   {
      disconnect();
   }
}

bool
JuliusNetworkClient::connect(const std::string& host, const unsigned short port)
{
   if (!mConnected)
   {
      ACE_INET_Addr addr(port, host.c_str(), AF_INET);
      ACE_Time_Value timeout(120);
      if (mConnector.connect(mDataStream, addr, &timeout) == -1)
      {
         return false;
      }
      mConnected = true;
   }
   return true;
}

void
JuliusNetworkClient::disconnect()
{
   if (mConnected)
   {
      /// The ACE documentation claims that close_writer() must be called
      /// first on Win32 platforms.
      mDataStream.close_writer();
      mDataStream.close();
      mConnected = false;
   }
}

bool
JuliusNetworkClient::startDataLoop()
{
   if (!mParser || !mConnected)
   {
      return false;
   }
   while (mConnected)
   {
      iovec data = {0, 0};
      ACE_Time_Value timeout(ACE_Time_Value::max_time);
      ssize_t read = mDataStream.recvv(&data, &timeout);
      if (read > 0 && data.iov_len > 0)
      {
         std::string data_to_parse = reinterpret_cast<char*>(data.iov_base);
         mParser->parse(data_to_parse);
         delete[] reinterpret_cast<char*>(data.iov_base);
      }
   }
   return true;
}
