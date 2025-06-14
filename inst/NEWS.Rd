\name{NEWS}
\title{News for Package 'hetid'}
\encoding{UTF-8}

\section{Changes in hetid version 0.1.0.9000 (development)}{
  \subsection{Infrastructure and Development Environment (2025-01-01)}{
    \itemize{
      \item \strong{New Features}
      \itemize{
        \item Added comprehensive ignore files following 2025 best practices:
        \itemize{
          \item Updated \code{.gitignore} with patterns from GitHub's official R template
          \item Restructured \code{.Rbuildignore} with clear sections and modern patterns
          \item Added \code{.cursorignore} for Cursor AI editor (security & performance)
          \item Added \code{.dockerignore} for optimized Docker builds
        }
      }
      
      \item \strong{Development Tools}
      \itemize{
        \item Configured pre-commit hooks for code quality:
        \itemize{
          \item R code formatting with \code{styler}
          \item Code linting with \code{lintr}
          \item Spell checking
          \item File formatting (trailing whitespace, end-of-file)
          \item Prevention of large files and R artifacts in commits
        }
        \item Set up GitHub Actions workflow:
        \itemize{
          \item R CMD check on multiple platforms
          \item Automated testing
          \item Documentation building
        }
      }
      
      \item \strong{Documentation}
      \itemize{
        \item Updated README.md with:
        \itemize{
          \item Comprehensive project structure
          \item Development environment setup instructions
          \item Information about new ignore files
          \item Current development status
        }
        \item Enhanced CONTRIBUTING.md with:
        \itemize{
          \item Detailed development workflow
          \item Code style guidelines
          \item Testing requirements
          \item Documentation standards
        }
      }
      
      \item \strong{Initial Package Structure}
      \itemize{
        \item Basic package skeleton with:
        \itemize{
          \item Utility functions (\code{messager.R}, \code{stopper.R})
          \item Test infrastructure (\code{testthat})
          \item Vignette structure (\code{getting-started.Rmd})
          \item pkgdown configuration
        }
      }
    }
  }
}

\section{Changes in hetid version 0.1.0 (future)}{
  \subsection{Overview}{
    \itemize{
      \item This will be the first release of hetid, implementing the core functionality of Lewbel (2012) identification through heteroskedasticity method.
    }
  }
  
  \subsection{Planned Features}{
    \itemize{
      \item Core Lewbel (2012) estimator implementation
      \item Functions for heteroskedasticity testing
      \item Time-series adaptations
      \item Comprehensive documentation and vignettes
      \item Full test coverage
    }
  }
} 