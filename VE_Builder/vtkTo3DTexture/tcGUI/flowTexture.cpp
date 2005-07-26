#include "flowTexture.h"
#include <iostream>
#include <fstream>
#include <vtkZLibDataCompressor.h>
#include <vtkUnsignedCharArray.h>

//////////////////////////////
//FlowPointData class       //
//////////////////////////////

//////////////////////////////
//Constructors              //
//////////////////////////////
FlowPointData::FlowPointData()
{
   //_data = new float[4];
   _data[0] = 0;
   _data[1] = 0;
   _data[2] = 0;
   _data[3] = 0;
   _dType = VECTOR;
   _valid = 1;
   
}
//////////////////////////////////////////////////////
FlowPointData::FlowPointData(const FlowPointData& fpd)
{
   //_data = new float[4];

   //set the type
   setDataType(fpd._dType);

   _data[0] = fpd._data[0];
   _data[1] = fpd._data[1];
   _data[2] = fpd._data[2];
   _data[3] = fpd._data[3];
   _valid = fpd._valid;
}
///////////////////////////////
//Destructor                 //
///////////////////////////////
FlowPointData::~FlowPointData()
{
   /*if(_data){
      delete [] _data;
      _data = 0;
   }*/
   
}
////////////////////////////////////////
//set the data at this flow point     //
////////////////////////////////////////
void FlowPointData::setData(float val0,
                            float val1,
                            float val2,
                            float val3)
{
   /*if(!_data){
      _data = new float[4];
   }*/
   _data[0] = val0;
   _data[1] = val1;
   _data[2] = val2;
   _data[3] = val3;


}
///////////////////////////////////////
//equal operator                     //
///////////////////////////////////////
FlowPointData& FlowPointData::operator=
              (const FlowPointData& rhs)
{
   if(this != &rhs){
      setData(rhs._data[0],
              rhs._data[1],
              rhs._data[2],
              rhs._data[3]);
      _dType = rhs._dType;
      _valid = rhs._valid;
   }
   return *this;
}

///////////////////////////////
//FlowTexture class          //
///////////////////////////////
///////////////////////////////
//Constructors               //
///////////////////////////////
FlowTexture::FlowTexture()
{
   _nPixels = 0;
   _dType = VECTOR;
   _bbox[0] = -1;
   _bbox[1] = 1;
   _bbox[2] = -1;
   _bbox[3] = 1;
   _bbox[4] = -1;
   _bbox[5] = 1;
}
///////////////////////////////////////////////
FlowTexture::FlowTexture(const FlowTexture& ft)
{
   _nPixels = ft._nPixels;
 
   _dType = ft._dType;
   _dims[0] = ft._dims[0];
   _dims[1] = ft._dims[1];
   _dims[2] = ft._dims[2];

   _bbox[0] = ft._bbox[0];
   _bbox[1] = ft._bbox[1];
   _bbox[2] = ft._bbox[2];
   _bbox[3] = ft._bbox[3];
   _bbox[4] = ft._bbox[4];
   _bbox[5] = ft._bbox[5];

   for(int i = 0; i < _nPixels; i++){
      _pointData.push_back(ft._pointData[i]);
   }
}
///////////////////////////
//Destructor             //
///////////////////////////
FlowTexture::~FlowTexture()
{
   if(_pointData.size()){
      _pointData.clear();
   }
}
////////////////////////////////////////////
//set the resolution of the texture       //
////////////////////////////////////////////
void FlowTexture::setTextureDimension(int x,
		                      int y,
				      int z)
{
   _dims[0] = x;
   _dims[1] = y;
   _dims[2] = z;
}
/////////////////////////////////////////////
void FlowTexture::setBoundingBox(double* bbox)
{
   _bbox[0] = bbox[0];
   _bbox[1] = bbox[1];
   _bbox[2] = bbox[2];
   _bbox[3] = bbox[3];
   _bbox[4] = bbox[4];
   _bbox[5] = bbox[5];
}
//////////////////////////////////////////////////
//i == x location                               //
//j == y location                               //
//k == z location                               //
//set the data at a pixel                       //
//////////////////////////////////////////////////
void FlowTexture::addPixelData(FlowPointData fpd)
{
   _pointData.push_back(fpd);
}
////////////////////////////////////////////
//get pixel data                          //
////////////////////////////////////////////
FlowPointData& FlowTexture::pixelData(int col,
	                               int row,
                                  int depth)
{
   if(!_dims[0]||!_dims[1]){
      std::cout<<"Invalid texture dimensions!!"<<std::endl;
      std::cout<<_dims[0]<<" "<<_dims[1]<<" "<<_dims[2]<<std::endl;
   }

   return _pointData.at(depth*(_dims[1]*_dims[0])
                   + _dims[0]*row + col);
}
//////////////////////////////////////////////
//write out the flow texture data to        //
//an ascii file it is an rgba file          //
//w/ float data                           //
//////////////////////////////////////////////
void FlowTexture::writeFlowTexture(char* file,
                               double* dataRange,
                               float* velMinMax)
{
   int pixelNum = 0;
   std::ofstream fout(file,std::ios::out);
   if(!fout.is_open()){
      std::cout<<"Couldn't write to directory: "<<file<<std::endl;
      return;
   }
   if(_pointData.size()){
      //scalar or vector data
      if(_pointData.at(0).type() == FlowPointData::SCALAR){
         fout<<"s"<<std::endl;
      }else{
         fout<<"v"<<std::endl;
      }
      if(!_dims[2])_dims[2] = 1;

      //range of the data values
      if(!dataRange)fout<<"0 0"<<std::endl;
      else fout<<dataRange[0]<<" "<<dataRange[1]<<std::endl;
      
      //bounding box
      fout<<_bbox[0]<<" "<<_bbox[1]<<" ";
      fout<<_bbox[2]<<" "<<_bbox[3]<<" ";
      fout<<_bbox[4]<<" "<<_bbox[5]<<std::endl;

      //texture dimensions
      fout<<_dims[0]<<" "<<_dims[1]<<" "<<_dims[2]<<std::endl;

      //loop through and quantize the data
      for(int k = 0; k < _dims[2]; k++){
         for(int j = 0; j < _dims[1]; j++){
            for(int i = 0; i < _dims[0]; i++){
            
               pixelNum = k*(_dims[0]*_dims[1])
		          + _dims[0]*j + i;
               fout<<_pointData[pixelNum];
               
            }
            fout<<std::endl;
         }
      }
   }
}

void FlowTexture::CreatFlowTextureBuffer(char* file,
                               double* dataRange,
                               float* velMinMax)
{
   int pixelNum = 0;
   std::ofstream fout(file,std::ios::out);
   if(!fout.is_open()){
      std::cout<<"Couldn't write to directory: "<<file<<std::endl;
      return;
   }
   std::ostringstream dataBuffer;
   if(_pointData.size())
   {
      //scalar or vector data
      if(_pointData.at(0).type() == FlowPointData::SCALAR)
      {
         dataBuffer<<"s"<<std::endl;
      }
      else
      {
         dataBuffer<<"v"<<std::endl;
      }
      
      if(!_dims[2])
         _dims[2] = 1;

      //range of the data values
      if(!dataRange)
         dataBuffer<<"0 0"<<std::endl;
      else 
         dataBuffer<<dataRange[0]<<" "<<dataRange[1]<<std::endl;
      
      //bounding box
      dataBuffer<<_bbox[0]<<" "<<_bbox[1]<<" ";
      dataBuffer<<_bbox[2]<<" "<<_bbox[3]<<" ";
      dataBuffer<<_bbox[4]<<" "<<_bbox[5]<<std::endl;

      //texture dimensions
      dataBuffer<<_dims[0]<<" "<<_dims[1]<<" "<<_dims[2]<<std::endl;

      //loop through and quantize the data
      for(int k = 0; k < _dims[2]; k++)
      {
         for(int j = 0; j < _dims[1]; j++)
         {
            for(int i = 0; i < _dims[0]; i++)
            {
               pixelNum = k*(_dims[0]*_dims[1])
		          + _dims[0]*j + i;
               dataBuffer<<_pointData[pixelNum];
               
            }
            dataBuffer<<std::endl;
         }
      }
   }
   // do data compressor

   // Compress the data
   vtkZLibDataCompressor* compressor = vtkZLibDataCompressor::New();
   vtkUnsignedCharArray* compressed_data = vtkUnsignedCharArray::New();
   const unsigned char* uncompressed_data;
   uncompressed_data = reinterpret_cast< const unsigned char* >( dataBuffer.str().c_str() );
   unsigned long grid_data_length = sizeof( uncompressed_data );
   compressed_data = compressor->Compress( uncompressed_data, grid_data_length );
   // send char* to file opened earlier
   fout << compressed_data->GetPointer( 0 );
   // close file
   fout.close();
}

////////////////////////////////////
//equal operator                  //
////////////////////////////////////
FlowTexture& FlowTexture::operator=
               (const FlowTexture& rhs)
{
   if(this != &rhs){
      _dims[0] = rhs._dims[0];
      _dims[1] = rhs._dims[1];
      _dims[2] = rhs._dims[2];
      _nPixels = 0;

      _pointData.clear();

      for(int i = 0; i < _nPixels; i++){
         _pointData.push_back(rhs._pointData[i]);
      }
   }
   return *this;
}
