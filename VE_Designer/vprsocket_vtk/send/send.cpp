#include <vpr/IO/Socket/SocketStream.h>
#include <vpr/System.h>
#include <vpr/vprTypes.h>
#include <vtk/vtkDataWriter.h>
#include <vtk/vtkUnstructuredGrid.h>
#include <vtk/vtkUnstructuredGridWriter.h>
#include <vtk/vtkUnstructuredGridReader.h>
#include <ostream>
#include <cstdlib>
#include "vtkSmartPtr.h"

using namespace vtkutil;

int main(int argc, char* argv[])
{
   vpr::SocketStream socket;
   vpr::InetAddr remote_addr;
   if(argc!=2)
   {
      return 1;
   }

   remote_addr.setAddress(argv[1], 50031);

   vtkSmartPtr<vtkUnstructuredGridReader> ugrid;
   ugrid->SetFileName("geometry.vtk");
   ugrid->Update();
   
   ///Create a writer
   vtkSmartPtr<vtkUnstructuredGridWriter> writer;
   writer->SetInput(ugrid->GetOutput());
   writer->SetFileTypeToBinary();
   writer->WriteToOutputStringOn();
   writer->Write();

   ///Get the Grid Data
   vpr::Uint8* grid_data = writer->GetBinaryOutputString();
   vpr::Uint32   grid_data_length = writer->GetOutputStringLength();
   
   vpr::Uint32 data_length = vpr::System::Htonl(grid_data_length);
   vpr::ReturnStatus status;
   ///Open a Socket

   char answer;
   
   std::cout<<" Do you want to send data to remote server ? (y/n)"<<std::endl;
   std::cin>>answer;

   while( answer =='y' )
   {
            socket.setRemoteAddr(remote_addr);
      socket.open();
      socket.connect();
      unsigned int bytes_sent =0;
   
      status = socket.send( (void*)(&data_length), sizeof(data_length), 
                         bytes_sent);
      if (status != vpr::ReturnStatus::Succeed)
      {
         std::cout << "[ERR] Unable to send length of data to remote host." 
                << std::endl;
      }
      else
      {
         std::cout << "[DBG] Sending " << grid_data_length << " bytes..." 
                << std::endl;
         status = socket.send(grid_data, grid_data_length, bytes_sent);
         if (status != vpr::ReturnStatus::Succeed)
         {
            std::cout << "[ERR] Unable to send data to remote host." << std::endl;
         }
         std::cout << "[DBG] Send " << bytes_sent << " of " << grid_data_length 
                << " bytes." << std::endl;
      }
      socket.close();
      std::cout<<" Do you want to send data to remote server ? (y/n)"<<std::endl;
      std::cin>>answer;

   }
   
   return 0;
}


