// Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

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

        private class SkippedNonRecursiveRewriter : CSharpNonRecursiveSyntaxRewriter
        {
            private SkipLiteralNonRecursiveRewriter _skipRewriter = new SkipLiteralNonRecursiveRewriter();

            protected override SkipVisitor Skip
            {
                get { return _skipRewriter; }
            }

            public int VisitNodeCallsCount { get; private set; }

            public override SyntaxNode VisitNode(SyntaxNode original, SyntaxNode rewritten)
            {
                this.VisitNodeCallsCount++;
                return base.VisitNode(original, rewritten);
            }

            private class SkipLiteralNonRecursiveRewriter : SkipVisitor
            {
                public override SkipRewrite VisitLiteralExpression(LiteralExpressionSyntax node)
                {
                    return new SkipRewrite(true, node);
                }
            }
        }

        private class SkippedAndTransformedNonRecursiveRewriter : CSharpNonRecursiveSyntaxRewriter
        {
            private SkipLiteralNonRecursiveRewriter _skipRewriter = new SkipLiteralNonRecursiveRewriter();

            protected override SkipVisitor Skip
            {
                get { return _skipRewriter; }
            }

            public int VisitNodeCallsCount { get; private set; }

            public override SyntaxNode VisitNode(SyntaxNode original, SyntaxNode rewritten)
            {
                this.VisitNodeCallsCount++;
                return base.VisitNode(original, rewritten);
            }

            private class SkipLiteralNonRecursiveRewriter : SkipVisitor
            {
                public override SkipRewrite VisitLiteralExpression(LiteralExpressionSyntax node)
                {
                    return new SkipRewrite(true, SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal(0)));
                }
            }
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
        public void TestNonRecursiveWalker()
        {
            string code = "if (a)\r\n  b .  Foo();";
            StatementSyntax statement = SyntaxFactory.ParseStatement(code);
            Assert.Equal(code, new ToStringVisitor().Visit(statement));
        }

        [Fact]
        public void TestNonRecursiveRewriterSkip()
        {
            string code = "1 + 2 + 3";
            ExpressionSyntax expression = SyntaxFactory.ParseExpression(code);
            var skippedNonRecursiveRewriter = new SkippedNonRecursiveRewriter();
            skippedNonRecursiveRewriter.Visit(expression);
            Assert.Equal(2, skippedNonRecursiveRewriter.VisitNodeCallsCount);
        }

        [Fact]
        public void TestNonRecursiveRewriterSkipAndTransform()
        {
            string code = "1 + 2 + 3";
            ExpressionSyntax expression = SyntaxFactory.ParseExpression(code);
            var skippedAndTransformedNonRecursiveRewriter = new SkippedAndTransformedNonRecursiveRewriter();
            var transformedExpr = skippedAndTransformedNonRecursiveRewriter.Visit(expression);
            Assert.Equal(2, skippedAndTransformedNonRecursiveRewriter.VisitNodeCallsCount);
            Assert.Equal("0+ 0+ 0", transformedExpr.ToString());
        }
    }
}
