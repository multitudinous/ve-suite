#include "vtk/vtkActor.h"
#include "vtk/vtkDataSetMapper.h"
#include "vtk/vtkUnstructuredGrid.h"
#include "vtk/vtkUnstructuredGridReader.h"
#include "vtk/vtkRenderer.h"
#include "vtk/vtkRenderWindow.h"
#include "vtk/vtkRenderWindowInteractor.h"
#include "vtk/vtkDataWriter.h"

#include "vtkSmartPtr.h"
#include <vpr/IO/Socket/SocketStream.h>
#include <vpr/IO/Socket/SocketAcceptor.h>
#include <vpr/System.h>
#include <vpr/vprTypes.h>

#include <iostream>
#include <cstring>
#include <cstdlib>

using namespace vtkutil;

int main(int argc, char* argv[])
{
   vpr::Uint32 data_length = 0;
   unsigned int bytes_read  = 0;
   
   vpr::InetAddr addr;
   vpr::SocketStream connection;
   
   if (argc != 2)
	{
		return 1;
	}
   
   addr.setAddress(argv[1],50031);
   vpr::SocketAcceptor server(addr);
   server.accept(connection);
   vpr::ReturnStatus status;
   status = connection.recvn( (void*)(&data_length), sizeof(data_length), bytes_read);
   if(status != vpr::ReturnStatus::Succeed)
   {
      std::cerr << "[ERR] Unable to receive data length "
                << __FILE__ << ":" << __LINE__ << std::endl;
      return 1;
   }
   ///Set the byte-order
   data_length = vpr::System::Ntohl(data_length);
   vpr::Uint8* data = new vpr::Uint8[data_length];
   status = connection.recvn( (void*)data, data_length, bytes_read );
   
   if(status != vpr::ReturnStatus::Succeed)
   {
      std::cout << "[ERR] Error receiving data; read " << bytes_read << " of "
                << data_length << " bytes." << std::endl;
      if (status == vpr::ReturnStatus::Fail)
      {
         std::cout << "[ERR] Read failed." << std::endl;
      }
      else if (status == vpr::ReturnStatus::WouldBlock)
      {
         std::cout << "[ERR] This read would block the caller." << std::endl;
      }
      else if (status == vpr::ReturnStatus::Timeout)
      {
         std::cout << "[ERR] This read timed out." << std::endl;
      }
      else if (status == vpr::ReturnStatus::InProgress)
      {
         std::cout << "[ERR] This read is still in progress." << std::endl;
      }
      else if (status == vpr::ReturnStatus::NotConnected)
      {
         std::cout << "[ERR] The device is not connected." << std::endl;
      }
      else
      {
         std::cout << "[ERR] Unknown Result." << std::endl;
      }
      return 1;
   }
   std::cout << "[DBG] Read " << bytes_read << " of " << data_length << " bytes."
             << std::endl;
   vtkSmartPtr<vtkUnstructuredGridReader> reader;
   reader->ReadFromInputStringOn();
   reader->SetBinaryInputString( reinterpret_cast<char*>(data), 
                                 data_length );
   vtkSmartPtr<vtkUnstructuredGrid> ugrid(reader->GetOutput());
    
   std::cout << "[DBG] Now drawing" << std::endl;

   vtkSmartPtr<vtkDataSetMapper> umapper;
   umapper->SetInput(ugrid);   
  
   vtkSmartPtr <vtkActor> uactor;
   uactor->SetMapper(umapper);
   uactor->SetPosition(0, 0, 0);
   uactor->SetScale(0.2, 0.2, 0.2);

   vtkSmartPtr<vtkRenderer> ren1 ;
   vtkSmartPtr<vtkRenderWindow> renWin ;
   renWin->AddRenderer(ren1);

   vtkSmartPtr<vtkRenderWindowInteractor>iren ;
   iren->SetRenderWindow(renWin);

   ren1->AddActor(uactor);
   ren1->SetBackground(0,0,0); // Background color white

   renWin->SetSize(300,300);
   // interact with data
   renWin->Render();
   iren->Start();      
   
	return 0;
}


