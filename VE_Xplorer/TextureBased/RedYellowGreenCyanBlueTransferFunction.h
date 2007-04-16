#ifndef RED_YELLOW_GREE_CYAN_BLUE_LINEAR_TRANSFER_FUNCTION_H
#define RED_YELLOW_GREE_CYAN_BLUE_LINEAR_TRANSFER_FUNCTION_H
#include <map>
#include <vector>
#include "./TransferFunction.h"
class RYGCBLinearTF: public TransferFunction
{
public:
   ///Constructor
   ///\param s Power of 2 (ie, 2^n) resolution
   RYGCBLinearTF(unsigned int s=256);
   
   ///Copy Constructor
   RYGCBLinearTF(const RYGCBLinearTF& rhs);

   ///Destructor
   virtual ~RYGCBLinearTF();

   ///Initialize the data in the function
   virtual void InitializeData();

   ///Update the transfer function
   ///\param component The ComponentType\n 0 == "RED"\n1 == "GREEN"\n2 == "BLUE"\n3 == "ALPHA"
   ///\param data A pointer for passing in user data
   ///\param rangeMin The starting value to update
   ///\param rangeMax The ending value to update
   virtual void Update(unsigned int component,
                       void* data,
                       float rangeMin=0.0,
                       float rangeMax=1.0);
   
   ///Equal operator
   ///\param rhs The TransferFunction to set this one to. 
   RYGCBLinearTF& operator=(const RYGCBLinearTF& rhs);
protected:
};
#endif// RED_YELLOW_GREE_CYAN_BLUE_LINEAR_TRANSFER_FUNCTION_H
