/*
    Author: E. David Huckaby
    Org:    NETL
            huckaby@netl.doe.gov
    Date:   3/25/2004
*/
#include <cstdio>
//#include <ctype.h>
#include "fluentIO.h"

namespace FluentReader {


FluentIO::~FluentIO() 
{ 
    if (m_inputFile)
        fclose(m_inputFile); 
    if (m_checkFile) 
        fclose(m_checkFile); 
    if (m_infoFile)
        fclose(m_infoFile); 
}

void FluentIO::init(const char *input, const char *info, const char *check, 
    bool isBinary, bool isGzip)
{
    if (isBinary)
        m_inputFile = fopen(input,"rb");
    else
        m_inputFile = fopen(input,"r");
    
    m_infoFile = fopen(info,"w");
    if (check)
        m_checkFile = fopen(check,"w");        
    else
        m_checkFile = 0;

    m_isDouble = true;
    m_treeLevel = 0;
    m_maxTreeLevel = 0;
    m_isBinary = isBinary;
}


/*
    search through the input file to determine a character
*/
int FluentIO::findChar(int character_to_find)
{
    int c = 0;  
    if (m_checkFile) 
        fprintf(m_checkFile,"\tFinding Character = %i\n",character_to_find);
    while (c != character_to_find )
        {
        c = this->nextChar();
        this->writePos(c);        
        /*
        fprintf(c,"c[%6i] = %3i (%4i)\n",
                ftell(case_info->input_file),c,indent);          
        */
        }
    return c;
}
/*
    get the next decimal parameter from the file

        - useful to go to the subsection
*/



int FluentIO::readInt()
{
int dec_param;
int c = 0;  
int dec_base = 10;
int i = 0;

    dec_param = 0;  
    if (m_checkFile)
        fprintf(m_checkFile,"\tBuilding Decimal Parameter\n");
    c = this->nextChar();
/* look for first integer */
    while ( (c != EOF) && (isdigit(c) == 0) )
        {
        c = this->nextChar();
        this->writeIndent(c); 
        printf("%x\n",c);
        }

    while ( (c != EOF) && (isdigit(c) != 0) )
        {
        dec_param = dec_param*dec_base + (c - '0');         
        this->writeParam(c, dec_param);                            
        c = this->nextChar();
        }    
    this->writePos(c);
    if (m_checkFile)
        fprintf(m_checkFile,"\t\tDecimal Parameter = %i\n",dec_param);
    return dec_param;
}
/*
    get the next "nparams" parameters from the file

        - useful to get information about the size of the current subsection
*/
int FluentIO::readIntParamList(int params[], int nParams)
{
      int j;
      for (j = 0 ; j < nParams ; j++ )
          {       
          params[j] = this->readInt();
          }
    return -1;
}

/*
    get the next hex-decimal parameter from the file
        - I think its only used the "get_hex_param_list"       
*/
int FluentIO::readHex()
{
int hex_param;
int c = 0;  
int hex_base = 16;

    hex_param = 0;  
    if (m_checkFile)
        fprintf(m_checkFile,"\tBuilding Hex Parameter\n");
    c = this->nextChar();

    while ( (c != EOF) && (isxdigit(c) == 0) )
        {
        c = this->nextChar();
        this->writeIndent(c);   
        printf("%x\n",c);   
        }

    while ( (c != EOF) && (isxdigit(c) != 0) )
        {
        if ( c <= '9') 
            {
            hex_param = hex_param*16 + (c - '0');     
            }
        else
            {
            hex_param = hex_param*16 + (c - 'a') + 10;     
            }
        this->writeParam(c, hex_param);                          
        c = this->nextChar();
        }
    this->writePos(c); 
    if (m_checkFile)
        fprintf(m_checkFile,"\t\tHex Parameter = %i, %x\n",hex_param,hex_param);
    return hex_param;
}

/*
    get the next "nparams" parameters from the file
        - useful to get information about the size of the current subsection
*/
int FluentIO::readHexParamList(int params[], int nparams)
{
    for (int j = 0 ; j < nparams ; j++ )
        {       
        params[j] = this->readHex();
        }
    return 0;
}

double *FluentIO::readDoubleArray(int size)
{
    double *array;
    this->findChar(OPEN_PAR);     /* data begins after close parenthesis of the meta-data */
    int ierr; 
    if (m_checkFile != NULL)
         fprintf(m_checkFile,"\tReading %i doubles (%i)  \n", size );         
 
    array = new double[size];
 
    if (m_isBinary)
        {             
        ierr = this->readChunk(array, sizeof(double), size); 
        } 
    else
        {
        for (int j = 0 ; j < size ; j++ )
            {
            fscanf(m_inputFile,"%lf",&array[j]);
            }
        }
    this->findChar(CLOSE_PAR);        
    return array;
}

float *FluentIO::readFloatArray(int size)
{
    float *array;
    this->findChar(OPEN_PAR);     /* data begins after close parenthesis of the meta-data */
    int ierr; 
    if (m_checkFile != NULL)
         fprintf(m_checkFile,"\tReading %i floats(%i) \n", size, sizeof(float) );         
 
    array = new float[size];
    
    if (m_isBinary)
        {             
        ierr = this->readChunk(array, sizeof(float), size);
        } 
    else
        {
        for (int j = 0 ; j < size ; j++ )
            {
            fscanf(m_inputFile,"%f",&array[j]);
            }
        }
    this->findChar(CLOSE_PAR);        
    return array;
}

void FluentIO::readRealArray(int size, double array[] )
{
    this->findChar(OPEN_PAR);     /* data begins after close parenthesis of the meta-data */
    int ierr; 
    if (m_checkFile != NULL)
         fprintf(m_checkFile,"\tReading %i floats(%i) \n", size, sizeof(double) );         
 
    //array = new float[size];
    
    if (m_isBinary)
        {             
        if (m_isDouble)
            ierr = this->readChunk(array, sizeof(double), size);
        else {
            float *float_array = new float[size];
            ierr = this->readChunk(float_array, sizeof(float), size);
            for (int j = 0; j < size; j++)
                array[j] = float_array[j];
            }
        } 
    else
        {
        for (int j = 0 ; j < size ; j++ )
            {
            double x,y;            
            fscanf(m_inputFile," %lf ",&x); 
            array[j] = x;
            if (m_checkFile)
                fprintf(m_checkFile,"\t\t%i\t%f\n",j,array[j]);            
            }
        }
    this->findChar(CLOSE_PAR);

}

int *FluentIO::readIntArray(int size)
{    
    int *array;
    this->findChar(OPEN_PAR);     /* data begins after close parenthesis of the meta-data */
    int ierr; 
    if (m_checkFile != NULL)
         fprintf(m_checkFile,"Reading %i integers (%i)  \n", size, sizeof(int) );         
 
    array = new int[size];
    if (m_isBinary)
        {             
        ierr = this->readChunk(array, sizeof(int), size ); 
        } 
    else
        {
        for (int j = 0 ; j < size ; j++ )
            {
            fscanf(m_inputFile,"%x",&array[j]); //fscanf(m_inputFile,"%x",&array[j]);
            } 
        }
    this->findChar(CLOSE_PAR);        
    return array; 
}


/* constRecordSize is because fluent often stores only variable size information 

for example a face thread with mixed sized elements will always have two-faces
but a variable number of nodes 
*/


/* convert freads to this->readChunk, this allows to imbed gzip calls inside the function */

int **FluentIO::readIntArray(int size, int constRecordSize)
{
    int **array_list;
    int *array;
    int k,j;
    int record_size;

    this->findChar(OPEN_PAR);
    if (m_checkFile != NULL)
    fprintf(m_checkFile,
        "Reading %i vectors of variable sized integers  \n",size);         
   
    array_list = new int*[size];
    std::cout << "is binary = " << m_isBinary << std::endl;
    
    if (m_isBinary)
        {
        for (j = 0; j < size; j++)
            {
            k = fread(&record_size, sizeof(int), 1, m_inputFile ); 
            
            record_size = record_size + constRecordSize;
            
            array = new int[record_size + 1];
            array[0] = record_size;
            array_list[j] = array;
            k = fread(&array[1], sizeof(int), record_size, m_inputFile );      
            }
        }
    else
        {
        for (j = 0; j < size; j++)
            {
            printf("vector number = %i of %i at pos = %i\n",j,size,this->pos());
            fscanf(m_inputFile,"%i",&record_size);
            printf("\tvector size = %i\n",record_size);        
            record_size = record_size + constRecordSize;
            array_list[j] = new int[record_size + 1];
            array_list[j][0] = record_size;            
            for (k = 0; k < record_size ; k++ )
                {                
                fscanf(m_inputFile,"%x",&array_list[j][k+1]);
                }
            }
        }

    this->findChar(CLOSE_PAR);
    return array_list;
}

/*

reads in data of the form

   nvertex  v0 v1 .. vn  cl cr

*/
intArray_2 FluentIO::readIntArray2(int nRecords, int maxRecordSize, int constRecordSize)
{
    //int temp_array[maxRecordSize];
    int *temp_array;
	temp_array = (int *) malloc(maxRecordSize);
    int k,j;
    int record_size;

    this->findChar(OPEN_PAR);
    if (m_checkFile != NULL)
    fprintf(m_checkFile, "Reading %i vectors of variable sized integers  \n",nRecords);         
   
    //array_list = new int*[size];
    intArray_2 array(nRecords, maxRecordSize);

    if (m_isBinary)
        {
        for (j = 0; j < nRecords; j++)
            {            
            k = fread(&record_size, sizeof(int), 1, m_inputFile ); 
            array(j,0) = record_size;                        
            /*k = fread(&temp_array, sizeof(int), record_size + constRecordSize , m_inputFile );                              
            for (i = 0; i < records_size + constRecordSize; i++)
                array(j,i+1) = temp_array[i];                                        
            */
            if ( record_size + constRecordSize >= maxRecordSize )
                { printf("there could be a problem in FluentIO::readIntArray, \n"); }
            fread( &array(j,1), sizeof(int), record_size + constRecordSize , m_inputFile );
            }
        }
    else
        {
        for (j = 0; j < nRecords; j++)
            {
            printf("vector number = %i of %i at pos = %i\n",j,nRecords,this->pos());
            fscanf(m_inputFile,"%i",&record_size);
            printf("\tvector size = %i\n",record_size);        
            //record_size = record_size + constRecordSize;
            array(j,0) = record_size;      
            if ( record_size + constRecordSize >= maxRecordSize )
                { printf("there could be a problem in FluentIO::readIntArray, \n"); }     
            for (k = 0; k < record_size + constRecordSize ; k++ )
                fscanf(m_inputFile,"%x",&array(j,k+1) );
                
            }
        }

    this->findChar(CLOSE_PAR);
    return array;
}

int FluentIO::getKeyword()
{

    int keyword_int = 0;
    int c;
    int indent = -999;
    while (  (c = this->nextChar()) != EOF && isdigit(c) )
        {
        keyword_int = keyword_int*10 + (c - '0');   /* subsection keywords are base10 numbers (not hex) */
        this->writeIndent(c);
        }
    this->writeIndent(c);

    if (c != ' ')
        {
        c = this->prevChar();
        this->writeIndent(c);
        }

    return keyword_int;

}


} /* end namespace FluentReader */

