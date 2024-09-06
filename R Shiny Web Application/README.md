R Shiny Web application:
The goal of the dashboard is to improve targeting efficiency by identifying high propensity customers for service attach and providing self-service capabilities for BDMs and sales teams. 
 

R Shiny Script Overview

This R Shiny script creates a web application with two main tabs: "Theater" and "Business Entity". Each tab features a dashboard with various metrics related to attach rates.

Common Structure
•	navbarPage ("Attach Rate"): The top-level navigation bar with two primary tabs: "Theater" and "Business Entity".

1. Theater Tab
	• Header: "Metrics by Theatre"
	• Sidebar:
		o Navigation items: Summary, Accounts View, Tabular, Predictive, Comparative, Dollar Per Unit
		o Input: selectInput to choose Region/Theater
	• Body:
		o Summary: Displays value boxes for various metrics and trends, with tables and plots.
		o Accounts View: Displays value boxes and a Waterfall View.
		o Tabular: Tables for top accounts, partners, product families, and business entities.
		o Predictive: Predictive metrics, trends, and tables.
		o Comparative: Comparative plots for regions/theaters.
		o Dollar Per Unit: Trends and tables for dollar per unit.

2. Business Entity Tab
	• Header: "Metrics by Business Entity"
	• Sidebar:
		o Navigation items similar to the "Theater" tab but for Business Entities
		o Input: SelectInput to choose Business Entity
	• Body:
		o Summary: Similar to the "Theater" summary but for Business Entities.
		o Accounts View: Similar to the "Theater" accounts view but for Business Entities.
		o Tabular: Similar to the "Theater" tabular but for Business Entities.
		o Predictive: Similar to the "Theater" predictive but for Business Entities.
		o Comparative: Comparison of metrics for Business Entities.
		o Dollar Per Unit: Similar to the "Theater" dollar per unit but for Business Entities.

Key Elements
	• dashboardPage: Used within each tab to define the layout with dashboardHeader, dashboardSidebar, and dashboardBody.
	• valueBoxOutput: Displays key metrics in summary.
	• plotOutput: Visualizes data trends and comparisons.
	• DT::DTOutput: Provides interactive tables for detailed views.
This script sets up a comprehensive dashboard for analyzing attach rates and related metrics across different regions and business entities.
