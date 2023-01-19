<template>
  <el-container>
    <el-header class="header">
      <el-breadcrumb class="breadcrumb" separator="/">
        <slot name="breadcrumbs"></slot>
      </el-breadcrumb>
      <div>
        <slot name="header-right"></slot>
      </div>
    </el-header>
    <el-main class="main">
      <slot></slot>
    </el-main>
  </el-container>
</template>

<script>
export default {
  name: "PageSkeleton",
}
</script>

<style>
.header {
  display: flex;
  justify-content: space-between;
  align-items: center;
}

.main {
  display: flex;
  flex-flow: column nowrap;
  align-items: stretch;

}

.breadcrumb {
  font-size: 18px;
}
</style>
