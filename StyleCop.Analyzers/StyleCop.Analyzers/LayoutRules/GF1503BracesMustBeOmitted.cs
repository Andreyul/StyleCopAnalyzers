// Copyright (c) Tunnel Vision Laboratories, LLC. All Rights Reserved.
// Licensed under the Apache License, Version 2.0. See LICENSE in the project root for license information.

namespace StyleCop.Analyzers.LayoutRules
{
    using System;
    using System.Collections.Generic;
    using System.Collections.Immutable;
    using System.Linq;
    using Microsoft.CodeAnalysis;
    using Microsoft.CodeAnalysis.CSharp;
    using Microsoft.CodeAnalysis.CSharp.Syntax;
    using Microsoft.CodeAnalysis.Diagnostics;
    using StyleCop.Analyzers.Helpers;
    using StyleCop.Analyzers.Settings.ObjectModel;

    /// <summary>
    /// The opening and closing braces for a C# statement have not been omitted.
    /// </summary>
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    internal class GF1503BracesMustBeOmitted : DiagnosticAnalyzer
    {
        /// <summary>
        /// The ID for diagnostics produced by the <see cref="GF1503BracesMustBeOmitted"/> analyzer.
        /// </summary>
        public const string DiagnosticId = "GF1503";
        private const string Title = "Braces should be omitted";
        private const string MessageFormat = "Braces should be omitted";
        private const string Description = "The opening and closing braces for a C# statement have not been omitted.";
        private const string HelpLink = "https://mobirate.com/";

        private static readonly DiagnosticDescriptor Descriptor =
            new DiagnosticDescriptor(DiagnosticId, Title, MessageFormat, AnalyzerCategory.LayoutRules, DiagnosticSeverity.Warning, AnalyzerConstants.EnabledByDefault, Description, HelpLink);

        private static readonly Action<SyntaxNodeAnalysisContext> IfStatementAction = HandleIfStatement;
        private static readonly Action<SyntaxNodeAnalysisContext, StyleCopSettings> UsingStatementAction = HandleUsingStatement;

        /// <inheritdoc/>
        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics { get; } =
            ImmutableArray.Create(Descriptor);

        public static bool NeedReport(StatementSyntax childStatement, out BlockSyntax outStatement)
        {
            if (childStatement is BlockSyntax bs)
            {
                if (bs.Statements.Count == 1)
                {
                    var start = bs.OpenBraceToken.GetLineSpan().StartLinePosition.Line;
                    var end = bs.CloseBraceToken.GetLineSpan().StartLinePosition.Line;
                    if (end - start == 2)
                    {
                        var statement = bs.Statements[0];
                        if (IsSimpleOneLineStatement(statement))
                        {
                            outStatement = bs;
                            return true;
                        }
                    }
                }
            }

            outStatement = null;
            return false;
        }

        public static bool IsOneLineStatement(StatementSyntax statement)
        {
            FileLinePositionSpan lineSpan = statement.GetLineSpan();
            return lineSpan.StartLinePosition.Line == lineSpan.EndLinePosition.Line;
        }

        public static bool IsSimpleOneLineStatement(StatementSyntax statement)
        {
            if (statement is BlockSyntax)
            {
                return false;
            }

            return IsOneLineStatement(statement);
        }

        /// <inheritdoc/>
        public override void Initialize(AnalysisContext context)
        {
            context.ConfigureGeneratedCodeAnalysis(GeneratedCodeAnalysisFlags.None);
            context.EnableConcurrentExecution();

            context.RegisterSyntaxNodeAction(IfStatementAction, SyntaxKind.IfStatement);
            context.RegisterSyntaxNodeAction(ctx => CheckChildStatement(ctx, ((WhileStatementSyntax)ctx.Node).Statement), SyntaxKind.WhileStatement);
            context.RegisterSyntaxNodeAction(ctx => CheckChildStatement(ctx, ((ForStatementSyntax)ctx.Node).Statement), SyntaxKind.ForStatement);
            context.RegisterSyntaxNodeAction(ctx => CheckChildStatement(ctx, ((ForEachStatementSyntax)ctx.Node).Statement), SyntaxKind.ForEachStatement);
            context.RegisterSyntaxNodeAction(UsingStatementAction, SyntaxKind.UsingStatement);
        }

        private static void HandleIfStatement(SyntaxNodeAnalysisContext context)
        {
            var ifStatement = (IfStatementSyntax)context.Node;
            if (ifStatement.Parent.IsKind(SyntaxKind.ElseClause))
            {
                // this will be analyzed as a clause of the outer if statement
                return;
            }

            List<StatementSyntax> clauses = new List<StatementSyntax>();
            for (IfStatementSyntax current = ifStatement; current != null; current = current.Else?.Statement as IfStatementSyntax)
            {
                clauses.Add(current.Statement);
                if (current.Else != null && !(current.Else.Statement is IfStatementSyntax))
                {
                    clauses.Add(current.Else.Statement);
                }
            }

            int count = 0;
            foreach (var c in clauses)
            {
                if (c is BlockSyntax b && b.Statements.Count == 1 && IsSimpleOneLineStatement(b.Statements[0]))
                {
                    var start = b.OpenBraceToken.GetLineSpan().StartLinePosition.Line;
                    var end = b.CloseBraceToken.GetLineSpan().StartLinePosition.Line;
                    if (end - start == 2)
                    {
                        ++count;
                        continue;
                    }
                }
            }

            if (count == clauses.Count)
            {
                context.ReportDiagnostic(Diagnostic.Create(Descriptor, ifStatement.GetLocation()));
            }
        }

        private static void HandleUsingStatement(SyntaxNodeAnalysisContext context, StyleCopSettings settings)
        {
            var usingStatement = (UsingStatementSyntax)context.Node;
            if (settings.LayoutRules.AllowConsecutiveUsings && usingStatement.Statement.IsKind(SyntaxKind.UsingStatement))
            {
                return;
            }

            CheckChildStatement(context, usingStatement.Statement);
        }

        private static void CheckChildStatement(SyntaxNodeAnalysisContext context, StatementSyntax childStatement)
        {
            if (NeedReport(childStatement, out BlockSyntax outStatement))
            {
                context.ReportDiagnostic(Diagnostic.Create(Descriptor, outStatement.GetLocation()));
            }
        }
    }
}
