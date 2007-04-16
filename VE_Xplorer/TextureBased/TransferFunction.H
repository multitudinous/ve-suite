#ifndef TRANSFER_FUNCTION_H
#define TRANSFER_FUNCTION_H
#include <map>
#include <vector>
class TransferFunction
{
public:
   enum ComponentType{
                      LINEAR,
                      RAMP,
	                   STEP,
	                   CUBIC,
	                   QUAD,
	                   EXPONENTIAL,
	                   CUSTOM
                      };
   ///Constructor
   ///\param dimension 1-3 are valid values.
   ///\param s Power of 2 (ie, 2^n) resolution
   ///\param t Power of 2 (ie, 2^n) resolution
   ///\param r Power of 2 (ie, 2^n) resolution
   TransferFunction(unsigned int dimension=1,
                    unsigned int s=256,
		              unsigned int t = 1,
		              unsigned int r = 1);
   
   ///Copy constructor
   TransferFunction(const TransferFunction& rhs);

   ///Destructor
   virtual ~TransferFunction();

   ///Initialize the data in the function
   virtual void InitializeData()=0;

   ///Set the components general "shape"
   ///\param type The ComponentType
   ///\param component The ComponentType\n 0 == "RED"\n1 == "GREEN"\n2 == "BLUE"\n3 == "ALPHA"
   void SetComponentType(ComponentType type, unsigned int component);

   ///Update the transfer function
   ///\param component The ComponentType\n 0 == "RED"\n1 == "GREEN"\n2 == "BLUE"\n3 == "ALPHA"
   ///\param data A pointer for passing in user data
   ///\param updateRangeMin The starting value to update
   ///\param updateRangeMax The ending value to update
   virtual void Update(unsigned int component,
                       void* data,
                       float updateRangeMin=0.0,
                       float updateRangeMax=1.0)=0;

   ///Get the resolution in a particular direction (r,s,t)
   ///\param direction 0 == s\n 1 == t\n 2 == r\n
   unsigned int GetResolution(unsigned int direction);

   ///Get the general "shape" for a specific component
   ///\param component The ComponentType\n 0 == "RED"\n1 == "GREEN"\n2 == "BLUE"\n3 == "ALPHA"
   ComponentType GetComponentType(unsigned int component);

   ///Evaluate the transfer function
   ///\param index The input to the transfer function
   float* EvaluateAt(unsigned int index);

   ///Get the dimension of the transfer function\n
   unsigned int GetDimension();

   ///Get the unsigned char data
   unsigned char* GetDataForTexture();

   ///Get the texture data
  inline unsigned char& unsignedByteDataAt (int i)
  { 
	  return _textureData[i]; 
  }
   ///Equal operator
   ///\param rhs The TransferFunction to set this one to. 
   TransferFunction& operator=(const TransferFunction& rhs);

   class UpdateCallback
   {
   public:
      UpdateCallback(TransferFunction* tf){_tf = tf;}
      virtual ~UpdateCallback(){}
      virtual void UpdateData()=0;
   protected:
      TransferFunction* _tf;
   };

   ///Set the transfer function update callback
   ///\param tfUpdate Set the update callback
   void SetUpdateCallback(UpdateCallback* tfUpdate);

   ///Get the update callback
   UpdateCallback* GetUpdateCallback();
protected:

   ///Set the resolution (power of 2) of the TF in a particular dimension
   ///\param direction "S","T","R" 
   ///\param resolution Power of 2 (ie, 2^n) resolution
   void _setResolution(unsigned int direction,
                       unsigned int resolution);
   ///Set the dimension of the transfer function\n
   ///\note Once created the dimensions CANNOT be modified.\n This is to protect from corrupting GL because this data is used for textures
   ///\param dimension 1-3 are valid values.
   void _setDimension(unsigned int dimension);

   unsigned int _dimension;///<The dimension of the transfer function
   unsigned int _resolution[3];///<The resolution in s,t,r directions.
   ComponentType _types[4];///<The RGBA component Types 
   /*std::map<std::string,std::vector<float> >*/
   float* _classification;///<The lookup values for RGBA
   unsigned char* _textureData;///<The texture data for representing the TF

   UpdateCallback* _updateCallback;///<Callback function to update the data in the TF
};
#define TransferFunction1D(X) TransferFunction(1,X,1,1)
#endif// TRANSFER_FUNCTION_H
