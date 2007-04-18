#include "VE_Xplorer/TextureBased/TransferFunction.h"
using namespace VE_TextureBased;
//////////////////////////////////////////////////////////
//Constructor                                           //
//////////////////////////////////////////////////////////
TransferFunction::TransferFunction(unsigned int dimension,
                                           unsigned int s,
                                           unsigned int t,
                                           unsigned int r )
{
   _classification = 0;
   _textureData = 0;
   _resolution[0] = s;
   _resolution[1] = t;
   _resolution[2] = r;
   _dimension = dimension;
   _types[0] = LINEAR;
   _types[1] = LINEAR;
   _types[2] = LINEAR;
   _types[3] = LINEAR;
   _updateCallback = 0;

   _originalScalarRange[0] = 0.0;
   _originalScalarRange[1] = 1.0;

   _currentScalarRange[0] = 0.0;
   _currentScalarRange[1] = 1.0;
   _isoSurface = false;
   _percentIsoValue = 0.;
}
///////////////////////////////////////////////////////////////
TransferFunction::TransferFunction(const TransferFunction& rhs)
{
   _resolution[0] = rhs._resolution[0];
   _resolution[1] = rhs._resolution[1];
   _resolution[2] = rhs._resolution[2];
   _dimension = rhs._dimension;
   _types[0] = rhs._types[0];
   _types[1] = rhs._types[1];
   _types[2] = rhs._types[2];
   _types[3] = rhs._types[3];
   _updateCallback = rhs._updateCallback;   
   _classification = new float[_resolution[0]*_resolution[1]*_resolution[2]*4];
   _textureData = new unsigned char[_resolution[0]*_resolution[1]*_resolution[2]*4];

   unsigned int resolution = _resolution[0]*_resolution[1]*_resolution[3];
   for(unsigned int i = 0; i < resolution; i++)
   {
      _classification[i] = rhs._classification[i];
      _textureData[i] = rhs._textureData[i];
   }
   
   _originalScalarRange[0] = rhs._originalScalarRange[0];
   _originalScalarRange[1] = rhs._originalScalarRange[1];

   _currentScalarRange[0] = rhs._currentScalarRange[0];
   _currentScalarRange[1] = rhs._currentScalarRange[1];
   _isoSurface = rhs._isoSurface;
   _percentIsoValue = rhs._percentIsoValue;
}
/////////////////////////////////////////////
//Destructor                            //
/////////////////////////////////////////////
TransferFunction::~TransferFunction()
{
   if(_classification)
   {
      delete [] _classification;
      _classification = 0;
   }
   if(_textureData)
   {
      delete [] _textureData;
      _textureData = 0;
   }
}
/////////////////////////////////////////////////////////////////////////////////
void TransferFunction::SetComponentType(ComponentType type,
                                                      unsigned int component)
{
   _types[component] = type;
}
/////////////////////////////////////////////////////////////////////
/*void TransferFunction::Update(unsigned int component,
                                      void* data,
                                      float rangeMin,
                                      float rangeMax)
{
}*/
///////////////////////////////////////
void TransferFunction::_update()
{
}
///////////////////////////////////////////////////////////////////
void TransferFunction::SetIsoSurface(bool isIsosurface)
{
	_isoSurface = isIsosurface;
	//_update();
}
/////////////////////////////////////////////////////////////////////////
void TransferFunction::SetIsoSurfaceValue(float percentage)
{
	_percentIsoValue = percentage;
	_update();
}
/////////////////////////////////////////////////////////////////////////////////////////
void TransferFunction::SetFullScalarRange(float minValue,float maxValue)
{
	_originalScalarRange[0] = minValue;
	_originalScalarRange[1] = maxValue;
	_update();
}
//////////////////////////////////////////////////////////////////////////
void TransferFunction::AdjustScalarMinimum(float minValue)
{
   _currentScalarRange[0] = minValue;
   _update();
}
///////////////////////////////////////////////////////////////////////////
void TransferFunction::AdjustScalarMaximum(float maxValue)
{
   _currentScalarRange[1] = maxValue;
   _update();
}
////////////////////////////////////////////////////////////////////////////////////
unsigned int TransferFunction::GetResolution(unsigned int direction)
{
   return _resolution[direction];
}
///////////////////////////////////////////////////////////////////////////////
TransferFunction::ComponentType 
TransferFunction::GetComponentType(unsigned int component)
{
   return _types[component];
}
/////////////////////////////////////////////////////////
unsigned int TransferFunction::GetDimension()
{
   return _dimension;
}
///////////////////////////////////////////////////////////////////////////
void TransferFunction::_setResolution(unsigned int direction,
                                               unsigned int resolution)
{
   _resolution[direction] = resolution;
}
////////////////////////////////////////////////////////////////////////////
void TransferFunction::_setDimension(unsigned int dimension)
{
   _dimension = dimension;
}
//////////////////////////////////////////////////////////////////
unsigned char* TransferFunction::GetDataForTexture()
{
   if(_textureData)
   {
      return _textureData;
   }
   return 0;
}
///////////////////////////////////////////////////////////////////
float* TransferFunction::EvaluateAt(unsigned int index)
{
   if(_classification)
   {
      return &_classification[index];
   }
   return 0;
}

//////////////////////////////////////////////////////////////////////////////////////////////////////////
void TransferFunction::SetUpdateCallback(TransferFunction::UpdateCallback* tfUpdate)
{
   _updateCallback = tfUpdate;
}
//////////////////////////////////////////////////////////////////////////////////////////
TransferFunction::UpdateCallback* TransferFunction::GetUpdateCallback()
{
   if(_updateCallback)
   {
      return _updateCallback;
   }
   return 0;
}
//////////////////////////////////////////////////////////////////////////
TransferFunction& TransferFunction::operator=(const TransferFunction& rhs)
{
   if(this != &rhs)
   {
      _resolution[0] = rhs._resolution[0];
      _resolution[1] = rhs._resolution[1];
      _resolution[2] = rhs._resolution[2];
      _dimension = rhs._dimension;
      _types[0] = rhs._types[0];
      _types[1] = rhs._types[1];
      _types[2] = rhs._types[2];
      _types[3] = rhs._types[3];
      _updateCallback = rhs._updateCallback;
      if(_classification)
      {
         delete [] _classification;
         _classification = 0;
      }

      if(_textureData)
      {
         delete [] _textureData;
         _textureData = 0;
      }
      _classification = new float[_resolution[0]*_resolution[1]*_resolution[2]*4];
      _textureData = new unsigned char[_resolution[0]*_resolution[1]*_resolution[2]*4];

      unsigned int resolution = _resolution[0]*_resolution[1]*_resolution[3];
      for(unsigned int i = 0; i < resolution; i++)
      {
         _classification[i] = rhs._classification[i];
         _textureData[i] = rhs._textureData[i];
      }
	  _originalScalarRange[0] = rhs._originalScalarRange[0];
      _originalScalarRange[1] = rhs._originalScalarRange[1];

      _currentScalarRange[0] = rhs._currentScalarRange[0];
      _currentScalarRange[1] = rhs._currentScalarRange[1];
      _isoSurface = rhs._isoSurface;
	  _percentIsoValue = rhs._percentIsoValue;
   }
   return *this;
}

