﻿// Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Linq;
using System.Text;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Xunit;

namespace Microsoft.CodeAnalysis.CSharp.UnitTests
{
    public partial class RedNodeTests
    {
        private class TokenDeleteRewriter : CSharpSyntaxRewriter
        {
            public override SyntaxToken VisitToken(SyntaxToken token)
            {
                return SyntaxFactory.MissingToken(token.Kind());
            }
        }

        private class IdentityRewriter : CSharpSyntaxRewriter
        {
            public override SyntaxNode DefaultVisit(SyntaxNode node)
            {
                return node;
            }
        }

        private class NonRecursiveTokenDeleteRewriter : CSharpNonRecursiveSyntaxRewriter
        {
            public override SyntaxToken VisitToken(SyntaxToken original, SyntaxToken rewritten)
            {
                return SyntaxFactory.MissingToken(rewritten.Kind());
            }
        }

        private class NonRecursiveIdentityRewriter : CSharpNonRecursiveSyntaxRewriter
        {
        }

        internal class ToStringVisitor : CSharpNonRecursiveSyntaxWalker
        {
            StringBuilder sb;

            public new string Visit(SyntaxNode node)
            {
                this.sb = new StringBuilder();
                base.Visit(node);
                return this.sb.ToString();
            }

            public override void VisitToken(SyntaxToken token)
            {
                sb.Append(token.ToFullString());
            }
        }

        [Fact]
        public void TestLongExpression()
        {
            var longExpression = Enumerable.Range(0, 1000000).Select(i => (ExpressionSyntax)SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal(i))).Aggregate((i, j) => SyntaxFactory.BinaryExpression(SyntaxKind.AddExpression, i, j));

            Assert.Throws(typeof(InsufficientExecutionStackException), () => new IdentityRewriter().Visit(longExpression));
        }

        [Fact]
        public void TestLongExpressionWithNonRecursiveRewriter()
        {
            var longExpression = Enumerable.Range(0, 1000000).Select(i => (ExpressionSyntax)SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal(i))).Aggregate((i, j) => SyntaxFactory.BinaryExpression(SyntaxKind.AddExpression, i, j));

            var exp = new NonRecursiveIdentityRewriter().Visit(longExpression);
            StringBuilder sb = new StringBuilder();
            for (int i = 0; i < 1000000; i++)
            {
                sb.Append(i);
                sb.Append("+");
            }
            sb.Length -= 1;
            Assert.Equal(sb.ToString(), exp.ToString());
        }

        [Fact]
        public void TestNonRecursiveFullTreeVisitor()
        {
            string code = "if (a)\r\n  b .  Foo();";
            StatementSyntax statement = SyntaxFactory.ParseStatement(code);
            Assert.Equal(code, new ToStringVisitor().Visit(statement));
        }
    }
}
