"use strict";(self.webpackChunkhydra_head_protocol_docs=self.webpackChunkhydra_head_protocol_docs||[]).push([[204],{3905:(e,t,n)=>{n.d(t,{Zo:()=>d,kt:()=>h});var r=n(67294);function a(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function o(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);t&&(r=r.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,r)}return n}function s(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?o(Object(n),!0).forEach((function(t){a(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):o(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function i(e,t){if(null==e)return{};var n,r,a=function(e,t){if(null==e)return{};var n,r,a={},o=Object.keys(e);for(r=0;r<o.length;r++)n=o[r],t.indexOf(n)>=0||(a[n]=e[n]);return a}(e,t);if(Object.getOwnPropertySymbols){var o=Object.getOwnPropertySymbols(e);for(r=0;r<o.length;r++)n=o[r],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(a[n]=e[n])}return a}var l=r.createContext({}),c=function(e){var t=r.useContext(l),n=t;return e&&(n="function"==typeof e?e(t):s(s({},t),e)),n},d=function(e){var t=c(e.components);return r.createElement(l.Provider,{value:t},e.children)},p="mdxType",m={inlineCode:"code",wrapper:function(e){var t=e.children;return r.createElement(r.Fragment,{},t)}},u=r.forwardRef((function(e,t){var n=e.components,a=e.mdxType,o=e.originalType,l=e.parentName,d=i(e,["components","mdxType","originalType","parentName"]),p=c(n),u=a,h=p["".concat(l,".").concat(u)]||p[u]||m[u]||o;return n?r.createElement(h,s(s({ref:t},d),{},{components:n})):r.createElement(h,s({ref:t},d))}));function h(e,t){var n=arguments,a=t&&t.mdxType;if("string"==typeof e||a){var o=n.length,s=new Array(o);s[0]=u;var i={};for(var l in t)hasOwnProperty.call(t,l)&&(i[l]=t[l]);i.originalType=e,i[p]="string"==typeof e?e:a,s[1]=i;for(var c=2;c<o;c++)s[c]=n[c];return r.createElement.apply(null,s)}return r.createElement.apply(null,n)}u.displayName="MDXCreateElement"},8464:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>l,contentTitle:()=>s,default:()=>m,frontMatter:()=>o,metadata:()=>i,toc:()=>c});var r=n(87462),a=(n(67294),n(3905));const o={sidebar_label:"End-to-End Benchmarks",sidebar_position:4},s="End-To-End Benchmark Results",i={unversionedId:"end-to-end-benchmarks",id:"end-to-end-benchmarks",title:"End-To-End Benchmark Results",description:"This page is intended to collect the latest end-to-end benchmarks  results produced by Hydra's Continuous Integration system from  the latest master code.",source:"@site/benchmarks/end-to-end-benchmarks.md",sourceDirName:".",slug:"/end-to-end-benchmarks",permalink:"/head-protocol/fr/benchmarks/end-to-end-benchmarks",draft:!1,editUrl:"https://github.com/input-output-hk/hydra/tree/master/docs/benchmarks/end-to-end-benchmarks.md",tags:[],version:"current",sidebarPosition:4,frontMatter:{sidebar_label:"End-to-End Benchmarks",sidebar_position:4},sidebar:"defaultSidebar",previous:{title:"Transactions Costs",permalink:"/head-protocol/fr/benchmarks/transaction-cost"},next:{title:"Profiling Hydra scripts",permalink:"/head-protocol/fr/benchmarks/profiling"}},l={},c=[{value:"Baseline Scenario",id:"baseline-scenario",level:2}],d={toc:c},p="wrapper";function m(e){let{components:t,...n}=e;return(0,a.kt)(p,(0,r.Z)({},d,n,{components:t,mdxType:"MDXLayout"}),(0,a.kt)("h1",{id:"end-to-end-benchmark-results"},"End-To-End Benchmark Results"),(0,a.kt)("p",null,"This page is intended to collect the latest end-to-end benchmarks  results produced by Hydra's Continuous Integration system from  the latest ",(0,a.kt)("inlineCode",{parentName:"p"},"master")," code."),(0,a.kt)("admonition",{type:"caution"},(0,a.kt)("p",{parentName:"admonition"},"Please take those results with a grain of  salt as they are currently produced from very limited cloud VMs and not controlled  hardware. Instead of focusing on the ",(0,a.kt)("em",{parentName:"p"},"absolute")," results, the emphasis  should be on relative results, eg. how the timings for a scenario  evolve as the code changes.")),(0,a.kt)("p",null,(0,a.kt)("em",{parentName:"p"},"Generated at"),"  2023-06-30 10:30:02.231543022 UTC"),(0,a.kt)("h2",{id:"baseline-scenario"},"Baseline Scenario"),(0,a.kt)("p",null,"This scenario represents a minimal case and as such is a good baseline against which  to assess the overhead introduced by more complex setups. There is a single hydra-node  with a single client submitting single input and single output transactions with a  constant UTxO set of 1."),(0,a.kt)("table",null,(0,a.kt)("thead",{parentName:"table"},(0,a.kt)("tr",{parentName:"thead"},(0,a.kt)("th",{parentName:"tr",align:null},"Number of nodes"),(0,a.kt)("th",{parentName:"tr",align:null},"3"))),(0,a.kt)("tbody",{parentName:"table"},(0,a.kt)("tr",{parentName:"tbody"},(0,a.kt)("td",{parentName:"tr",align:null},(0,a.kt)("em",{parentName:"td"},"Number of txs")),(0,a.kt)("td",{parentName:"tr",align:null},"90")),(0,a.kt)("tr",{parentName:"tbody"},(0,a.kt)("td",{parentName:"tr",align:null},(0,a.kt)("em",{parentName:"td"},"Avg. Confirmation Time (ms)")),(0,a.kt)("td",{parentName:"tr",align:null},"213.675036988")),(0,a.kt)("tr",{parentName:"tbody"},(0,a.kt)("td",{parentName:"tr",align:null},(0,a.kt)("em",{parentName:"td"},"Share of Txs (%) < 100ms")),(0,a.kt)("td",{parentName:"tr",align:null},"6.666666666666667")))))}m.isMDXComponent=!0}}]);