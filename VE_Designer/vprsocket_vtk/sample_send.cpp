#include <vpr/IO/Socket/Socket.h>
#include <vtk/IO/vtkDataWriter.h>
#include <ostream>

void sendVTKUnstructuredGrid( const vtkUnstructuredGrid& grid )
{
   std::stringbuf str_buffer;
   std::ostream output_stream(&str_buffer);
   vpr::Socket socket;
   vpr::InetAddr remote_addr;
   remote_addr.setAddress("127.0.0.1", 50031);
   ///Create a writer
   vtkSmartPtr<vtkUnstructuredGridWriter> writer;
   writer->SetInput(&grid);
   writer->SetFileTypeToBinary();
   writer->SetWriteToOutputStringOn();
   writer->Write();
   ///Get the Grid Data
   unsigned char* grid_data = writer->GetBinaryOutputString();
   unsigned int grid_data_length = writer->GetOutputStringLength();
   ///Open a Socket
   socket.open();
   socket.setRemoteAddr(remote_addr);
   socket.connect();
   unsigned int bytes_sent = 0;
   vpr::ReturnStatus status;
   status = socket.send(grid_data_length, sizeof(grid_data_length), bytes_sent);
   if (vpr::ReturnStatus::Succeed != status)
   {
      std::cout << "[ERR] Unable to send length of data to remote host." 
                << std::endl;
   }
   else
   {
      status = socket.send(grid_data, grid_data_length, bytes_sent);
      if (vpr::ReturnStatus::Succeed != status)
      {
         std::cout << "[ERR] Unable to send data to remote host." << std::endl;
      }
   }
   socket.close();
}

void recv()
{
   vpr::InetAddr addr;
   addr.setAddress("127.0.0.1", 50031);
   vpr::SocketAcceptor server( addr );
   vpr::Socket connection;
   server.accept(connection);
   unsigned int data_length = 0;
   unsigned int bytes_read = 0;
   vpr::ReturnStatus status;
   ///Get the length of the data
   status = server.recvn( (void *)(&data_length), sizeof(data_length), 
                           bytes_read );
   unsigned char* data = new char[data_length];
   ///Get the data.
   status = server.recvn( reinterpret_cast<void*>(data), data_length, 
                          bytes_read);
   vtkSmartPtr<vtkUnstructuredGridReader> reader;
   reader->SetBinaryInputString(data, data_length);
   vtkSmartPtr<vtkUnstructuredGrid> grid(reader->GetOutput());
}
