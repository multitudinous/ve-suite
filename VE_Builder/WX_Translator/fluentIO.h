/*
    Author: E. David Huckaby
    Org:    NETL
            huckaby@netl.doe.gov
    Date:   3/25/2004
*/

#ifndef FLUENT_IO
#define FLUENT_IO

#include "var_defs.h"
#include <string>

namespace FluentReader {

const int NEWLINE =  '\n';
const int CLOSE_PAR  = ')';
const int OPEN_PAR  = '(';

class FluentIO
{
    public:
        FluentIO(char *input, char *info, char *check, 
            bool isBinary, bool isGzip){ init(input,info,check,isBinary,isGzip); }
        FluentIO( std::string input, std::string info, std::string check, 
            bool isBinary, bool isGzip){ 
                if (check.size() == 0)
                    init(input.c_str(),info.c_str(),0,isBinary,isGzip); 
                else
                    init(input.c_str(),info.c_str(),check.c_str(),isBinary,isGzip); 
                }                 
        //FluentIO(char *filename);
        //FluentIO(string filename);
        void init(const char *input, const char *info, const char *check, bool isBinary, bool isGzip);
        ~FluentIO();

        int nextChar() { m_char = getc(m_inputFile); return m_char; }
        int prevChar() { ungetc(m_char, m_inputFile); return m_char; }
        int findChar(int character_to_find);
        int pos() { return ftell(m_inputFile); }
        void toChild() 
            { 
            m_treeLevel = m_treeLevel + 1; 
            if (m_maxTreeLevel < m_treeLevel) 
                m_maxTreeLevel = m_treeLevel;
            }
        void toParent() { m_treeLevel--; }
        int treeLevel() {return m_treeLevel; }
        void setTreeLevel(int level) {m_treeLevel = level; }
        void setMaxTreeLevel(int level) {m_maxTreeLevel = level; }

        void writeLine() { if (m_checkFile) fprintf(m_checkFile,"\n"); }

        void writeKeyword(char *keyword_text, int keyword_int)
            {
            if (m_infoFile != NULL)
                fprintf(m_infoFile,"'%s'\t%i\n",keyword_text,keyword_int); 
            if (m_checkFile != NULL)
                fprintf(m_checkFile,"'%s'\t%i\n",keyword_text,keyword_int); 
            };

        void writeParam(char c, int param) 
            {
            if (m_checkFile != NULL)
            fprintf( m_checkFile,"c[%6i] = %3i  dec_param = %5i\n",this->pos(), c, param); 
            }
        void writePos(char c)
            {
            if (m_checkFile != NULL)
                fprintf( m_checkFile,"c[%6i] = %3i\n",this->pos(), c); 
            }

        void writeOpen()
            {
            if (m_checkFile != NULL)
                fprintf(m_checkFile," [%i]_(\n",m_treeLevel);       
            }
            
        void writeClose()
            {
            if (m_checkFile != NULL)
                fprintf(m_checkFile, " %i)_[%i]\n",m_maxTreeLevel, m_treeLevel); 
            }

        void writeIndent(char c)
            {
            if (m_checkFile != NULL)
                fprintf( m_checkFile,"c[%6i] = %3i (%4i)\n",this->pos(), c, this->treeLevel() ); 
            }

        int readChunk( void *array, int data_size, int array_size)
            {
            return fread( array, data_size, array_size, m_inputFile );
            }

        int getKeyword();
        int readInt();
        int readHex();
        int readIntParamList(int params[], int nParams);
        int readHexParamList(int params[], int nParams);

        int getHex() { return readHex(); }
        int getInt() { return readInt(); }

        int *readIntArray(int size);
        float *readFloatArray(int size );
        double *readDoubleArray(int size );
	        
        //void readDoubleArray(int size, *double);
        void readRealArray(int size, double array[] );
        
    
        double *readRealArray(int size) { 
			if ( isDouble() )
				return readDoubleArray(size);
			else	
				{			
				float *float_array = readFloatArray(size);				
				double *double_array;
				double_array = new double[size];
				for (int i = 0; i < size; i++)					
					double_array[i] = (double) float_array[i];
				return double_array;
				}
			}
	
        int **readIntArray(int size, int constRecordSize);  
         
        intArray_2 readIntArray2(int nRecords, int maxRecordSize, int constRecordSize);

        FILE* checkFile() { return m_checkFile; }  
        FILE* infoFile() { return m_infoFile; }   
        int nDims() { return m_nDims; }
        void setDims(int nDims) { m_nDims = nDims; }
        
        bool isAscii() {return  !m_isBinary; }
        bool isBinary() {return m_isBinary; }
        bool isDouble() {return m_isDouble; }
        bool isFloat() {return !m_isDouble; }

        void enableFloat() {m_isDouble = false; }
        void enableDouble() {m_isDouble = true; }

	/*
        void incrementIndent() { 
            this->toChild();
            if (m_maxTreeLevel < m_treeLevel) m_maxTreeLevel = m_treeLevel;  }
        void decrementIndent() { this->toParent(); }
	*/
        /*
        int indent() { return m_indent(); }
        void setMaxIndent(int maxIndent) { m_maxIndent = maxIndent; }        
        */

    private:
        int m_treeLevel;        /* instaneous branch level of file */
        int m_maxTreeLevel;
        bool m_isBinary;        /* is input binary */
        bool m_isDouble;        /* is input double */
        FILE *m_inputFile;      /* fluent case or data file */
        FILE *m_checkFile;      /* character-by-character output for checking (text) */
        FILE *m_infoFile;       /* information about size and location
                                             of objects (text) */
        int m_nDims;
        int m_char;        
};

}
#endif
