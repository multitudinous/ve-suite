#ifndef CASI_H
#define CASI_H

//#include <happ.h>

#ifndef _AFXDLL
#define _AFXDLL
#endif

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#define VC_EXTRALEAN		// Exclude rarely-used stuff from Windows headers

#include <afxwin.h>         // MFC core and standard components
#include <afxext.h>         // MFC extensions
#include <afxdisp.h>        // MFC Automation classes
#include <afxdtctl.h>		// MFC support for Internet Explorer 4 Common Controls
#ifndef _AFX_NO_AFXCMN_SUPPORT
#include <afxcmn.h>			// MFC support for Windows Common Controls
#endif // _AFX_NO_AFXCMN_SUPPORT

#include <set>
#include <vector>
#include <map>

#import "happ.tlb"

#  ifdef CASI_LIBRARY
#    define CASI_EXPORTS   __declspec(dllexport)
#  else
#    define CASI_EXPORTS   __declspec(dllimport)
#  endif // CASI_EXPORTS

namespace CASI
{

///////////////////////////////////////////////////////////////////////

	typedef enum { STREAM, BLOCK, VARIABLE } VARTYPE;
	
	class CASI_EXPORTS CASIBroker
	{
		public:

		CASIBroker(); //default constructor 

		void init(); //initialization should go here. 
		//Put this call here instead of make it in constructor is to avoid some weire DLL potential problem
		void fini(); //finalize the broker;
	};


	class CASI_EXPORTS Variable
	{
		//This class mimic the right panel of the Variable Exploer
		public:

		Variable(): nodePath(""), aliasName(""), nodeType(VARIABLE) {};
        Variable(Happ::IHNodePtr root, CString nodePath, VARTYPE nodeType=VARIABLE);
        
        //Block Node and Stream Node get special treatment
		VARTYPE getNodeType();
        //the Full Path to the node
		CString getNodePath();
        //the variable name user associated with
		CString getAliasName();
        //the variable explorer name of the variable, which is the lastpart of the full path
		CString getName();
        //how many child this node have
		int getNumChild();
        //return a pointer to the indexed child
		Variable getChild(int index);
		 //The string display in the value entry box;
		CString getValue();
        //The string display in the PhysicalQuantity box
		CString getPhysicalQuantity();
        //The string displayed in the Unit of Measure box
		CString getUnitOfMeasure();
         //The string displayed in the Basis box
		CString getBasis();
         //The string displayed in the Option List Box
  		CString getOptionList();
        //The string displayed in the Option Box, deliminate by /n for each option
		CString getOptions();
        //The string displayed in the Record Type Box;
		CString getRecordType();

		long getDimension();
 
        //The string in the Output Box, 1 means true, 0 means false
        CString isOutput();
 
        //The string in the Enterable Box, 1 means true, 0 means false
		CString isEnterable();
 
        //The string in the HasChild Box, 1 means true, 0 means false
		CString hasChild();
 
        //the up Limit of the value
		CString upLimit();
 
        //the lower limit of the value
		CString lowerLimit();
 
        //the default value
		CString getDefaultValue();
 
        //the prompt box
		CString getPrompt();

        //the completion status box, deliminated by \n, the string leading with& is the first one
		CString getCompletionStatus(); 
 
        //the In or Out box
		CString getInorOut();
 
        //the Gender box
		CString getGender();
 
        //the multiport box
		CString getMultiport();
 
        //the port type box
		CString getPortType();

		void setAliasName(CString aliasname); //the variable name user associated with
		bool setValue(CString val); //The value field is writable. And at least in GUI, set value could fail;

		bool valid();

		int getValues(CString* values);
		CString getVVVV(int index);
	protected:
		CString nodePath;
		CString aliasName;
		VARTYPE nodeType;
        Happ::IHNodePtr ihRoot;
	};


	class CASI_EXPORTS CASIObj : public Variable
	{
	public:
		CASIObj():Variable() {};
		CASIObj(Happ::IHNodePtr root, CString nodepath, VARTYPE nodetype);
		CString getInputVarName(int index); //get a varaible name in the input category by index
        int getNumberOfInputVars( );
		CString getOutputVarName(int index); //get a varaible name in the output category by index
        int getNumberOfOutputVars( );
		CString getSIPortName(int index); //get the source or input port name by index
		CString getDOPortName(int index); //get the destination or output port name by index

		Variable getOutputVarByName(CString); //get a pointer to a output variable by name
		Variable getInputVarByName(CString);	//get a pointer to a input variable by name
		Variable getInputVarByIndex(int);  //get a pointer to a input variable by index
		Variable getOutputVarByIndex(int); //get a pointer to a output variable by index
				
		int getNumCompOfStream(); //get number of the components in Table seciton for a certain stream
		CString* GetStreamComponentNameList(); //get the string list of stream component names
		Variable GetStreamComponentVarByName(CString streamName, CString componentName); //get the varialble pointer of a stream component
		CString GetStreamComponentVarValue(CString streamName, CString componentName); //get the value of a stream component
		bool SetStreamComponentVarValue(CString streamName, CString componentName, CString value); //set the value of a stream component

		int getNumInputVar(); //get number of input variable
		int getNumOutputVar(); //get number of output variable
        int getNumSubInputVar( CString );
        int getNumSubOutputVar( CString );
		int getNumSIPort(); //get number of source/input port
		int getNumDOPort(); //get number of destination/output port

		int getFVNVariables();

		private:
		void prepStream();
		void prepBlock();
		void processBlocks();
		void prepBlockInputSubs( CString, CString );
		void prepBlockOutputSubs( CString, CString );
		std::set< CString > streamComps;
		std::vector< CString > blockInputs;
		std::vector< CString > blockOutputs;
		std::vector< CString > blockInputsWithSubs;
		std::vector< CString > blockOutputsWithSubs;
        //std::map< CString, std::vector< CString > > blockInputsWithSubs;
		//std::map< CString, std::vector< CString > > blockOutputsWithSubs;
	};
		
	class CASI_EXPORTS CASIDocument
	{
	
		public:
		CASIDocument();
		~CASIDocument();
	
		//Interfacing function
		Happ::IHNodePtr getRoot();
		//File operating functions
		void open(CString filename); //Open an Aspen Document
		void close(); //Close the file, clear up 
		void save(); //Save the document back;
		void saveAs(CString filename); //save this as another document
		void showAspen(bool status);
		
		//Block, Stream and variable access functions
		int getNumOfBlocks(); //get number of blocks in this document
		int getNumOfStreams(); //get number of streams in the this document
		
		CString* getStreamNames(); //get the string list of the stream names
		CString* getBlockNames(); //get the string list of the block names

		CASIObj getUpStreamBlock(const CASIObj stream); //get a stream's upstream block
		CASIObj getDownStreamBlock(const CASIObj stream); //get a stream's downstream block

		CASIObj getBlockInletStream(const CASIObj block, int portIndex, int streamIndex); //get a block's inlet stream
		CASIObj getBlockOutletStream(const CASIObj block, int portIndex, int streamIndex); //get a block's outlet stream
	
		CASIObj getBlockByName(CString blockName); //get a block pointer by name
		CASIObj getBlockByIndex(int index); //get a block pointer by index

		CASIObj getStreamByName(CString); //get a stream pointer by the stream 
		CASIObj getStreamByIndex(int index); //get a stream pointer by index

		Variable getVarByAlias(CString); //get a pointer to a variable by alias name
		Variable getVarByNodePath(CString path); //get a viable pointer by the node path in the variable expoloer
		
		//simulation control fucntions
		
		void initializeSolver(); //Initializes the current solution to its initial state. May purge the current results form the problem.
        void reinitializeBlock( CString block );

        void runSolver(bool async);
		void step();
		
		//call backs
		virtual void solved();  //callback when a problem is solved
		virtual void failed();  //callback when a problem solving is failed
	
		private:

		Happ::IHNodePtr ihRoot;
		Happ::IHappPtr hAPsim;
		CString nodePath;
		std::vector<CASIObj> blocks;
		std::vector<CASIObj> streams;
		std::map<CString, CString> aliasMapping;
		bool simOpened;

		//Now it is the new Dummy node approach.
		//This is due to Steve Zitney's Email that a dummery node inserted 
		//in "Data\Flowsheeting Options\Design-Spec" will create a structure for input and output
		void CreateDummyDesignSpec();
		
	};

	//Utility functions:
	Happ::IHNodePtr nodeNav(Happ::IHNodePtr root, CString NodeName); //navigation
	int getChildNum(Happ::IHNodePtr root);
	void getChildNames(Happ::IHNodePtr root, std::vector<CString>& results);

	::CString variantToString(VARIANTARG& var);

	void stringToVariant(CString val, VARIANTARG& var);

    void readTree(Happ::IHappPtr hAPsim);

	void readBranch(Happ::IHNodePtr root);

	void readStreamsAndBlocks(Happ::IHNodePtr root, std::vector<CASIObj> &blocks, std::vector<CASIObj> &streams);
}
#endif