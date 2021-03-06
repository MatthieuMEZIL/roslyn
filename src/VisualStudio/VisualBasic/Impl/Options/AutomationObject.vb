' Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

Imports System.Runtime.InteropServices
Imports System.Xml.Linq
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.CodeStyle
Imports Microsoft.CodeAnalysis.Editor.Shared.Options
Imports Microsoft.CodeAnalysis.ExtractMethod
Imports Microsoft.CodeAnalysis.Options
Imports Microsoft.CodeAnalysis.Shared.Options
Imports Microsoft.CodeAnalysis.Simplification

Namespace Microsoft.VisualStudio.LanguageServices.VisualBasic.Options
    <ComVisible(True)>
    Public Class AutomationObject
        Private ReadOnly _workspace As Workspace

        Friend Sub New(workspace As Workspace)
            _workspace = workspace
        End Sub

        Public Property AutoComment As Boolean
            Get
                Return GetBooleanOption(FeatureOnOffOptions.AutoXmlDocCommentGeneration)
            End Get
            Set(value As Boolean)
                SetBooleanOption(FeatureOnOffOptions.AutoXmlDocCommentGeneration, value)
            End Set
        End Property

        Public Property AutoEndInsert As Boolean
            Get
                Return GetBooleanOption(FeatureOnOffOptions.EndConstruct)
            End Get
            Set(value As Boolean)
                SetBooleanOption(FeatureOnOffOptions.EndConstruct, value)
            End Set
        End Property

        Public Property AutoRequiredMemberInsert As Boolean
            Get
                Return GetBooleanOption(FeatureOnOffOptions.AutomaticInsertionOfAbstractOrInterfaceMembers)
            End Get
            Set(value As Boolean)
                SetBooleanOption(FeatureOnOffOptions.AutomaticInsertionOfAbstractOrInterfaceMembers, value)
            End Set
        End Property

        <Obsolete("This SettingStore option has now been deprecated in favor of BasicClosedFileDiagnostics")>
        Public Property ClosedFileDiagnostics As Boolean
            Get
                Return ServiceFeatureOnOffOptions.IsClosedFileDiagnosticsEnabled(_workspace.Options, LanguageNames.VisualBasic)
            End Get
            Set(value As Boolean)
                ' Even though this option has been deprecated, we want to respect the setting if the user has explicitly turned off closed file diagnostics (which is the non-default value for 'ClosedFileDiagnostics').
                ' So, we invoke the setter only for value = False.
                If Not value Then
                    SetBooleanOption(ServiceFeatureOnOffOptions.ClosedFileDiagnostic, value:=0)
                End If
            End Set
        End Property

        Public Property BasicClosedFileDiagnostics As Integer
            Get
                Return GetBooleanOption(ServiceFeatureOnOffOptions.ClosedFileDiagnostic)
            End Get
            Set(value As Integer)
                SetBooleanOption(ServiceFeatureOnOffOptions.ClosedFileDiagnostic, value)
            End Set
        End Property

        Public Property RenameTrackingPreview As Boolean
            Get
                Return GetBooleanOption(FeatureOnOffOptions.RenameTrackingPreview)
            End Get
            Set(value As Boolean)
                SetBooleanOption(FeatureOnOffOptions.RenameTrackingPreview, value)
            End Set
        End Property

        Public Property DisplayLineSeparators As Boolean
            Get
                Return GetBooleanOption(FeatureOnOffOptions.LineSeparator)
            End Get
            Set(value As Boolean)
                SetBooleanOption(FeatureOnOffOptions.LineSeparator, value)
            End Set
        End Property

        Public Property EnableHighlightReferences As Boolean
            Get
                Return GetBooleanOption(FeatureOnOffOptions.ReferenceHighlighting)
            End Get
            Set(value As Boolean)
                SetBooleanOption(FeatureOnOffOptions.ReferenceHighlighting, value)
            End Set
        End Property

        Public Property EnableHighlightRelatedKeywords As Boolean
            Get
                Return GetBooleanOption(FeatureOnOffOptions.KeywordHighlighting)
            End Get
            Set(value As Boolean)
                SetBooleanOption(FeatureOnOffOptions.KeywordHighlighting, value)
            End Set
        End Property

        Public Property ExtractMethod_DoNotPutOutOrRefOnStruct As Boolean
            Get
                Return GetBooleanOption(ExtractMethodOptions.DontPutOutOrRefOnStruct)
            End Get
            Set(value As Boolean)
                SetBooleanOption(ExtractMethodOptions.DontPutOutOrRefOnStruct, value)
            End Set
        End Property

        Public Property ExtractMethod_AllowMovingDeclaration As Boolean
            Get
                Return GetBooleanOption(ExtractMethodOptions.AllowMovingDeclaration)
            End Get
            Set(value As Boolean)
                SetBooleanOption(ExtractMethodOptions.AllowMovingDeclaration, value)
            End Set
        End Property

        Public Property Outlining As Boolean
            Get
                Return GetBooleanOption(FeatureOnOffOptions.Outlining)
            End Get
            Set(value As Boolean)
                SetBooleanOption(FeatureOnOffOptions.Outlining, value)
            End Set
        End Property

        Public Property PrettyListing As Boolean
            Get
                Return GetBooleanOption(FeatureOnOffOptions.PrettyListing)
            End Get
            Set(value As Boolean)
                SetBooleanOption(FeatureOnOffOptions.PrettyListing, value)
            End Set
        End Property

        Public Property Style_PreferIntrinsicPredefinedTypeKeywordInDeclaration As Boolean
            Get
                Return GetBooleanOption(SimplificationOptions.PreferIntrinsicPredefinedTypeKeywordInDeclaration)
            End Get
            Set(value As Boolean)
                SetBooleanOption(SimplificationOptions.PreferIntrinsicPredefinedTypeKeywordInDeclaration, value)
            End Set
        End Property

        Public Property Style_PreferIntrinsicPredefinedTypeKeywordInMemberAccess As Boolean
            Get
                Return GetBooleanOption(SimplificationOptions.PreferIntrinsicPredefinedTypeKeywordInMemberAccess)
            End Get
            Set(value As Boolean)
                SetBooleanOption(SimplificationOptions.PreferIntrinsicPredefinedTypeKeywordInMemberAccess, value)
            End Set
        End Property

        Public Property Style_QualifyFieldAccess As String
            Get
                Return GetXmlOption(CodeStyleOptions.QualifyFieldAccess)
            End Get
            Set(value As String)
                SetXmlOption(CodeStyleOptions.QualifyFieldAccess, value)
            End Set
        End Property

        Public Property Style_QualifyPropertyAccess As String
            Get
                Return GetXmlOption(CodeStyleOptions.QualifyPropertyAccess)
            End Get
            Set(value As String)
                SetXmlOption(CodeStyleOptions.QualifyPropertyAccess, value)
            End Set
        End Property

        Public Property Style_QualifyMethodAccess As String
            Get
                Return GetXmlOption(CodeStyleOptions.QualifyMethodAccess)
            End Get
            Set(value As String)
                SetXmlOption(CodeStyleOptions.QualifyMethodAccess, value)
            End Set
        End Property

        Public Property Style_QualifyEventAccess As String
            Get
                Return GetXmlOption(CodeStyleOptions.QualifyEventAccess)
            End Get
            Set(value As String)
                SetXmlOption(CodeStyleOptions.QualifyEventAccess, value)
            End Set
        End Property

        Public Property Option_PlaceSystemNamespaceFirst As Boolean
            Get
                Return GetBooleanOption(OrganizerOptions.PlaceSystemNamespaceFirst)
            End Get
            Set(value As Boolean)
                SetBooleanOption(OrganizerOptions.PlaceSystemNamespaceFirst, value)
            End Set
        End Property

        Public Property Option_SuggestImportsForTypesInReferenceAssemblies As Boolean
            Get
                Return GetBooleanOption(AddImportOptions.SuggestForTypesInReferenceAssemblies)
            End Get
            Set(value As Boolean)
                SetBooleanOption(AddImportOptions.SuggestForTypesInReferenceAssemblies, value)
            End Set
        End Property

        Public Property Option_SuggestImportsForTypesInNuGetPackages As Boolean
            Get
                Return GetBooleanOption(AddImportOptions.SuggestForTypesInNuGetPackages)
            End Get
            Set(value As Boolean)
                SetBooleanOption(AddImportOptions.SuggestForTypesInNuGetPackages, value)
            End Set
        End Property

        Private Function GetBooleanOption(key As [PerLanguageOption](Of Boolean)) As Boolean
            Return _workspace.Options.GetOption(key, LanguageNames.VisualBasic)
        End Function

        Private Function GetXmlOption(key As PerLanguageOption(Of CodeStyleOption(Of Boolean))) As String
            Return _workspace.Options.GetOption(key, LanguageNames.VisualBasic).ToXElement().ToString()
        End Function

        Private Sub SetBooleanOption(key As [PerLanguageOption](Of Boolean), value As Boolean)
            _workspace.Options = _workspace.Options.WithChangedOption(key, LanguageNames.VisualBasic, value)
        End Sub

        Private Function GetBooleanOption(key As PerLanguageOption(Of Boolean?)) As Integer
            Dim [option] = _workspace.Options.GetOption(key, LanguageNames.VisualBasic)
            If Not [option].HasValue Then
                Return -1
            End If

            Return If([option].Value, 1, 0)
        End Function

        Private Sub SetBooleanOption(key As PerLanguageOption(Of Boolean?), value As Integer)
            Dim boolValue As Boolean? = If(value < 0, Nothing, value > 0)
            _workspace.Options = _workspace.Options.WithChangedOption(key, LanguageNames.VisualBasic, boolValue)
        End Sub

        Private Sub SetXmlOption(key As PerLanguageOption(Of CodeStyleOption(Of Boolean)), value As String)
            Dim convertedValue = CodeStyleOption(Of Boolean).FromXElement(XElement.Parse(value))
            _workspace.Options = _workspace.Options.WithChangedOption(key, LanguageNames.VisualBasic, convertedValue)
        End Sub

    End Class
End Namespace
