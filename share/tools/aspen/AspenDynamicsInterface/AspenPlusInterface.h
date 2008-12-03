#ifndef DASI_H
#define DASI_H

//#include <aspendynamics.h>

#include <set>
#include <vector>
#include <map>


#include <afxwin.h>         // MFC core and standard components
#include <afxext.h>         // MFC extensions
#include <afxdisp.h>        // MFC Automation classes
#include <afxdtctl.h>		// MFC support for Internet Explorer 4 Common Controls
#include <afxcmn.h>			// MFC support for Windows Common Controls


#  ifdef ASPENPLUSINTERFACE_LIBRARY
#    define ASPENPLUSINTERFACE_EXPORTS   __declspec(dllexport)
#  else
#    define ASPENPLUSINTERFACE_EXPORTS   __declspec(dllimport)
#  endif // DASI_EXPORTS

#import "aspendynamics.tlb"

namespace AspenPlusInterface
{

///////////////////////////////////////////////////////////////////////

/*	typedef enum { STREAM, BLOCK, VARIABLE } VARTYPE;
	
	class DASI_EXPORTS DASIBroker
	{
		public:

		DASIBroker(); //default constructor 

		void init(); //initialization should go here. 
		//Put this call here instead of make it in constructor is to avoid some weire DLL potential problem
		void fini(); //finalize the broker;
	};


	class DASI_EXPORTS Variable
	{
		//This class mimic the right panel of the Variable Exploer
		public:

		Variable(): nodePath(""), aliasName(""), nodeType(VARIABLE) {};
		Variable(IHNode root, CString nodePath, VARTYPE nodeType=VARIABLE);
		VARTYPE getNodeType(); //Block Node and Stream Node get special treatment

		CString getNodePath();//the Full Path to the node

		CString getAliasName(); //the variable name user associated with

		CString getName(); //the variable explorer name of the variable, which is the lastpart of the full path

		int getNumChild(); //how many child this node have

		Variable getChild(int index); //return a pointer to the indexed child
		
		CString getValue(); //The string display in the value entry box;
		CString getPhysicalQuantity(); //The string display in the PhysicalQuantity box
		CString getUnitOfMeasure(); //The string displayed in the Unit of Measure box
		CString getBasis(); //The string displayed in the Basis box
		CString getOptionList(); //The string displayed in the Option List Box
		CString getOptions(); //The string displayed in the Option Box, deliminate by /n for each option
		CString getRecordType(); //The string displayed in the Record Type Box;
		long getDimension();
        CString isOutput(); //The string in the Output Box, 1 means true, 0 means false
		CString isEnterable(); //The string in the Enterable Box, 1 means true, 0 means false
		CString hasChild(); //The string in the HasChild Box, 1 means true, 0 means false
		CString upLimit(); //the up Limit of the value
		CString lowerLimit(); //the lower limit of the value
		CString getDefaultValue(); //the default value
		CString getPrompt(); //the prompt box
		CString getCompletionStatus(); //the completion status box, deliminated by \n, the string leading with& is the first one
		CString getInorOut(); //the In or Out box
		CString getGender(); //the Gender box
		CString getMultiport(); //the multiport box
		CString getPortType(); //the port type box

		void setAliasName(CString aliasname); //the variable name user associated with
		bool setValue(CString val); //The value field is writable. And at least in GUI, set value could fail;

		bool valid();

		int getValues(CString* values);
		CString getVVVV(int index);
	protected:
		CString nodePath;
		CString aliasName;
		VARTYPE nodeType;
		IHNode ihRoot;
	};


	class DASI_EXPORTS DASIObj : public Variable
	{
	public:
		DASIObj():Variable() {};
		DASIObj(IHNode root, CString nodepath, VARTYPE nodetype);
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
		*/
	class ASPENPLUSINTERFACE_EXPORTS AspenPlusInterface
	{
	
		public:
		AspenPlusInterface();
		~AspenPlusInterface();
	
		//Interfacing function
		//IHNode getRoot();
		//File operating functions
		void Open(CString filename); //Open an Aspen Document
		void Close(); //Close the file, clear up 
		void Quit();
		void Save(); //Save the document back;
		void SaveAs(CString filename); //save this as another document
		void SetVisibility( bool status );
        void RunSolver( );
        void ResetSimulation( );
        //SAFEARRAY * GetVariableList( CString itemName );
        std::vector< std::vector< std::string > > GetVariableList( CString itemName );
        //std::vector< std::string > GetVariableList( CString itemName );
        void SetVariableValue( CString itemName, CString variableName,
            CString Value );


        private:
		bool simOpened;
        AspenDynamicsLibrary::IAspenModelerPtr ADApplication;
        AspenDynamicsLibrary::IAspenModelerDocumentPtr ADDocument;
        AspenDynamicsLibrary::IAspenModelerPhysicalPropertiesPtr ADProperties;
        AspenDynamicsLibrary::IAspenModelerResultsPtr ADResults;
		
		/*//Block, Stream and variable access functions
		int getNumOfBlocks(); //get number of blocks in this document
		int getNumOfStreams(); //get number of streams in the this document
		
		CString* getStreamNames(); //get the string list of the stream names
		CString* getBlockNames(); //get the string list of the block names

		DASIObj getUpStreamBlock(const DASIObj stream); //get a stream's upstream block
		DASIObj getDownStreamBlock(const DASIObj stream); //get a stream's downstream block

		DASIObj getBlockInletStream(const DASIObj block, int portIndex, int streamIndex); //get a block's inlet stream
		DASIObj getBlockOutletStream(const DASIObj block, int portIndex, int streamIndex); //get a block's outlet stream
	
		DASIObj getBlockByName(CString blockName); //get a block pointer by name
		DASIObj getBlockByIndex(int index); //get a block pointer by index

		DASIObj getStreamByName(CString); //get a stream pointer by the stream 
		DASIObj getStreamByIndex(int index); //get a stream pointer by index

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

		IHNode* ihRoot;
		IHapp* hAPsim;
		CString nodePath;
		std::vector<DASIObj> blocks;
		std::vector<DASIObj> streams;
		std::map<CString, CString> aliasMapping;
		bool simOpened;

		//Now it is the new Dummy node approach.
		//This is due to Steve Zitney's Email that a dummery node inserted 
		//in "Data\Flowsheeting Options\Design-Spec" will create a structure for input and output
		void CreateDummyDesignSpec();
		*/
	};

	//Utility functions:
	//IHNode nodeNav(IHNode& root, CString NodeName); //navigation
	//int getChildNum(IHNode &root);
	//void getChildNames(IHNode &root, std::vector<CString>& results);

	//::CString variantToString(VARIANTARG& var);

	//void stringToVariant(CString val, VARIANTARG& var);

	//void readTree(IHapp& hAPsim);

	//void readBranch(IHNode& root);

	//void readStreamsAndBlocks(IHNode& root, std::vector<DASIObj> &blocks, std::vector<DASIObj> &streams);
}
#endif
