VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "AspenDynamicsQueryLib"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
    Dim ADDispatch 'The Aspen Dynamics Dispatch
    
    'For connecting to an open flowsheet
    Public Function ConnectToDynamics(flowsheetName As String) As Boolean
    
       'Connect to the flowsheet
        Set ADDispatch = GetObject(flowsheetName)
        
        'Needs error checking
        ConnectToDynamics = True
        
    End Function

    'Grab the list of variables/values and put in safearray
    Public Function GetVariableList(itemName As String) As String()
        
        'Get the flowsheet object
        Set ADFlowsheet = ADDispatch.Application.Simulation.Flowsheet
        
        'Get a list of all variables with for an object
        Set ADVariables = ADFlowsheet.Blocks().Item(CStr(itemName)).FindMatchingVariables("~", "~", "", False, True, True, False)
        total = ADVariables.count
        Dim results() As String
        ReDim results(1 To total, 1 To 7) As String
        Dim count As Integer
        count = 1
        
        'loop over variables and add to 2d array (safearray)
        For Each i In ADVariables
            ADDispatch.Application.Msg ("Test GASIFIER-" & testString & "-Block " & i.Name & " uses Model " & i.TypeName & " " & i.Spec & " " & i.Value & " " & i.Description & " " & i.Units & " " & i.Tag & ".")
            results(count, 1) = i.Name
            results(count, 2) = i.TypeName
            results(count, 3) = i.Spec
            results(count, 4) = i.Value
            results(count, 5) = i.Description
            results(count, 6) = i.Units
            results(count, 7) = i.Tag
            count = count + 1
        Next
        
        'return array
        GetVariableList = results
        
    End Function
    
    'Set the value of a variable of an object
    Public Function SetVariableValue(itemName As String, variableName As String, variableValue As String) As Boolean
        
        'Get the flowsheet object
        Set ADFlowsheet = ADDispatch.Application.Simulation.Flowsheet
        
        'Grab the variable
        Set ADVariable = ADFlowsheet.Blocks().Item(CStr(itemName)).FindMatchingVariables(CStr(variableName), "~", "", False, True, True, False)
        
        'actually grabs a list of variables even through the list is one
        'still have to loop
        For Each i In ADVariable
            i.Value = CStr(variableValue)
        Next
        
        'Needs error checking
        SetVariableValue = True
        
    End Function
